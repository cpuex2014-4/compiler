type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec print_syntax outchan exp indent=
  let rec make_indent outchan i =
    if i > 0 
    then 
      (output_char outchan ' ';
       output_char outchan ' ';
       make_indent outchan (i-1))
  in 
  let out exp = 
    make_indent outchan indent;
    output_string outchan (exp^"\n") in
  match exp with
    | Unit -> out "Unit"
    | Bool bool -> out ("Bool "^string_of_bool bool)
    | Int int -> out ("Int "^string_of_int int)
    | Float float -> out ("Float "^string_of_float float)
    | Not t -> 
      (out "Not" ;
       print_syntax outchan t (indent + 1))
    | Neg t -> 
      (out "Neg" ;
       print_syntax outchan t (indent + 1))
    | Add (t0, t1) -> 
      (out "Add" ;
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | Sub (t0, t1) -> 
      (out "Sub" ;
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | FNeg t -> 
      (out "FNeg";
       print_syntax outchan t (indent + 1))
    | FAdd (t0, t1) ->
      (out "FAdd";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | FSub (t0, t1) ->
      (out "FSub";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | FMul (t0, t1) ->
      (out "FMul";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | FDiv (t0, t1) ->
      (out "FDiv";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | Eq (t0, t1) ->
      (out "Eq";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | LE (t0, t1) ->
      (out "LE";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | If (t0, t1, t2) ->
      (out "If";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1);
       print_syntax outchan t2 (indent + 1))
    | Let (name, t0, t1) ->
      (out "Let";
      print_name outchan name (indent + 1);
      print_syntax outchan t0 (indent + 1);
      print_syntax outchan t1 (indent + 1))
    | Var id -> out ("Var "^id)
    | LetRec (fundef, t) ->
      (out "LetRec";
       print_fundef outchan fundef (indent + 1);
       print_syntax outchan t (indent + 1))
    | App (t, tl) ->
      (out "App";
       print_syntax outchan t (indent + 1);
       print_syntax_list outchan tl (indent + 1))
    | Tuple tl ->
      (out "Tuple";
       print_syntax_list outchan tl (indent + 1))
    | LetTuple (namel, t0, t1) ->
      (out "LetTuple";
      print_name_list outchan namel (indent + 1);
      print_syntax outchan t0 (indent + 1);
      print_syntax outchan t1 (indent + 1))
    | Array (t0, t1) ->
      (out "Array";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | Get (t0, t1) ->
      (out "Get";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1))
    | Put (t0, t1, t2) ->
      (out "Put";
       print_syntax outchan t0 (indent + 1);
       print_syntax outchan t1 (indent + 1);
       print_syntax outchan t2 (indent + 1))
and print_syntax_list outchan tl indent =
  match tl with
    | [] -> ()
    | t::ts -> 
      (print_syntax outchan t indent;
       print_syntax_list outchan ts indent)
and print_name outchan name indent =
  let (id, ty) = name in
  Id.print_id outchan id indent
and print_name_list outchan namel indent =
  match namel with
    | [] -> ()
    | name::names ->
      (print_name outchan name indent;
       print_name_list outchan names indent)
and print_fundef outchan fundef indent =
  print_name outchan fundef.name indent;
  print_name_list outchan fundef.args indent;
  print_syntax outchan fundef.body indent
