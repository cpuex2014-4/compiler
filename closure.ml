type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = { name : Id.l * Type.t;
		args : (Id.t * Type.t) list;
		formal_fv : (Id.t * Type.t) list;
		body : t }
type prog = Prog of fundef list * t

let rec make_indent outchan i =
  if i > 0 
  then 
    (output_char outchan ' ';
     output_char outchan ' ';
     make_indent outchan (i-1))

let rec print_closure outchan exp indent =
  let out exp = 
    make_indent outchan indent;
    output_string outchan (exp^"\n")
  in
  match exp with
    | Unit -> ()
    | Int int -> out ("Int "^string_of_int int)
    | Float float -> out ("Float "^string_of_float float)
    | Neg id -> out ("Neg "^id)
    | Add (id0, id1) -> out ("Add "^id0^" "^id1)
    | Sub (id0, id1) -> out ("Sub "^id0^" "^id1)
    | Mul (id0, id1) -> out ("Sub "^id0^" "^id1)
    | Div (id0, id1) -> out ("Sub "^id0^" "^id1)
    | FNeg id -> out ("FNeg "^id)
    | FAdd (id0, id1) -> out ("FAdd "^id0^" "^id1)
    | FSub (id0, id1) -> out ("FSub "^id0^" "^id1)
    | FMul (id0, id1) -> out ("FMul "^id0^" "^id1)
    | FDiv (id0, id1) -> out ("FDiv "^id0^" "^id1)
    | IfEq (id0, id1, t0, t1) ->
      (out ("IfEq "^id0^" "^id1);
       print_closure outchan t0 (indent + 1);
       print_closure outchan t1 (indent + 1))
    | IfLE (id0, id1, t0, t1) ->
      (out ("IfLE "^id0^" "^id1);
       print_closure outchan t0 (indent + 1);
       print_closure outchan t1 (indent + 1))
    | Let ((id, _), t0, t1) ->
      (out "Let";
       Id.print_id outchan id (indent + 1);
       print_closure outchan t0 (indent + 1);
       print_closure outchan t1 (indent + 1))
    | Var id -> out ("Var "^id)
    | MakeCls ((id, _), cls, t) ->
      (out "MakeCls";
       Id.print_id outchan id (indent + 1);
       print_closure_sub outchan cls (indent + 1);
       print_closure outchan t (indent + 1))
    | AppCls (id, idl) ->
      (out "AppCls";
       Id.print_id outchan id (indent + 1);
       Id.print_id_list outchan idl (indent + 1))
    | AppDir (label, idl) ->
      (out "AppDir";
       Id.print_label outchan label (indent + 1);
       Id.print_id_list outchan idl (indent + 1))
    | Tuple idl ->
      (out "Tuple";
       Id.print_id_list outchan idl (indent + 1))
    | LetTuple (namel, id, t) ->
      (out "LetTuple";
       print_name_list outchan namel (indent + 1);
       print_closure outchan t (indent + 1))
    | Get (id0, id1) ->
      (out "Get";
       Id.print_id outchan id0 (indent + 1);
       Id.print_id outchan id1 (indent + 1))
    | Put (id0, id1, id2) ->
      (out "Put";
       Id.print_id outchan id0 (indent + 1);
       Id.print_id outchan id1 (indent + 1);
       Id.print_id outchan id2 (indent + 1))
    | ExtArray label ->
      (out "ExtArray";
       Id.print_label outchan label (indent + 1))
and print_name outchan name indent =
  let (id, t) = name in
  Id.print_id outchan id indent
and print_name_list outchan namel indent =
  match namel with
    | [] -> ()
    | name::res -> 
      (print_name outchan name indent;
       print_name_list outchan res indent)
and print_closure_sub outchan cls indent =
  Id.print_label outchan cls.entry indent;
  Id.print_id_list outchan cls.actual_fv indent

let print_fundef outchan fundef indent =
  let (label,_) = fundef.name in
  Id.print_label outchan label indent;
  print_name_list outchan fundef.args indent;
  print_name_list outchan fundef.formal_fv (indent + 1);
  print_closure outchan fundef.body indent

let rec print_fundef_list outchan fundefl indent =
  match fundefl with
    | [] -> ()
    | fundef::res ->
      (print_fundef outchan fundef indent;
       print_fundef_list outchan res indent)

let print_prog outchan fundef indent =
  match fundef with
    | Prog (fundefl, t) ->
      (print_fundef_list outchan fundefl indent;
       print_closure outchan t indent)
  
let rec fv = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

let rec g env known = function (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Mul(x, y) -> Mul(x, y)
  | KNormal.Div(x, y) -> Div(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> 
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
	if S.is_empty zs then known', e1' else
	(Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
	 Format.eprintf "function %s cannot be directly applied in fact@." x;
	 toplevel := toplevel_backup;
	 let e1' = g (M.add_list yts env') known e1 in
	 known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in 
      let zts = List.map (fun z -> (z, M.find z env')) zs in 
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; 
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then 
	MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') 
      else
	(Format.eprintf "eliminating closure(s) %s@." x;
	 e2') 
  | KNormal.App(x, ys) when S.mem x known -> 
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')
