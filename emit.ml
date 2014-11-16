open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* すでにSaveされた変数の集合 (caml2html: emit_stackset) *)
let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
    stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = -4 * List.hd (locate x)
let stacksize () = align (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> string_of_int i

(* 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
					 (function
					   | (y', z) when y = y' -> (sw, z)
					   | yz -> yz)
					 xys)
  | xys, acyc -> acyc @ shuffle sw xys

exception Shift_amount_is_not_4_error
exception Not_supported_yet
type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 (caml2html: emit_dest) *)
let rec g oc = function (* 命令列のアセンブリ生成 (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function (* 各命令のアセンブリ生成 (caml2html: emit_gprime) *)
  (* 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail) *)
  | NonTail(_), Nop -> ()
  | NonTail(x), Set(i) -> Printf.fprintf oc "\taddiu\t%s, $zero, %d\n" x i
  | NonTail(x), FSet(d) ->
    Printf.fprintf oc "\tli\t$at, 0x%lx\n" (getlo d);
    Printf.fprintf oc "\tmtc1\t$at, %s\n" x
  | NonTail(x), SetL(Id.L(y)) -> 
    Printf.fprintf oc "\taddiu\t%s, $zero, %s\n" x y
  | NonTail(x), Mov(y) ->
    if x <> y then Printf.fprintf oc "\taddu\t%s, %s, $zero\n" x y
  | NonTail(x), Neg(y) -> Printf.fprintf oc "\tsubu%s, $zero, %s\n" x y
  | NonTail(x), Add(y, V(z)) -> Printf.fprintf oc "\taddu\t%s, %s, %s\n" x y z
  | NonTail(x), Add(y, C(z)) -> Printf.fprintf oc "\taddiu\t%s, %s, %d\n" x y z
  | NonTail(x), Sub(y, V(z)) -> Printf.fprintf oc "\tsubu\t%s, %s, %s\n" x y z
  | NonTail(x), Sub(y, C(z)) -> Printf.fprintf oc "\taddiu\t%s, %s, %d\n" x y ((-1) * z)
  | NonTail(x), Ld(y, V(z), i) ->
    if i = 1 then
     (Printf.fprintf oc "\taddu\t$at, %s, %s\n" y z;
      Printf.fprintf oc "\tlw\t%s, 0($at)" x)
    else if i = 4 then 
    (Printf.fprintf oc "\tsll\t$at, %s, 2\n" z;
     Printf.fprintf oc "\taddu\t$at, $at, %s\n" y;
     Printf.fprintf oc "\tlw\t%s, 0($at)\n" x)
    else raise Shift_amount_is_not_4_error
  | NonTail(x), Ld(y, C(j), i) -> Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), St(x, y, V(z), i) -> 
    if i = 1 then
      (Printf.fprintf oc "\taddu\t$at, %s, %s\n" y z;
       Printf.fprintf oc "\tsw\t%s, 0($at)\n" x)
    else if i = 4 then 
      (Printf.fprintf oc "\tsll\t$at, %s, 2\n" z;
       Printf.fprintf oc "\taddu\t$at, $at, %s\n" y;
       Printf.fprintf oc "\tsw\t%s, 0($at)\n" x)
    else raise Shift_amount_is_not_4_error
  | NonTail(_), St(x, y, C(j), i) -> Printf.fprintf oc "\tsw\t%d(%s), %s\n" (j * i) y x
  | NonTail(x), FMovD(y) -> Printf.fprintf oc "\tmov.s\t%s, %s\n" x y
  | NonTail(x), FNegD(y) -> raise Not_supported_yet (* $f0 = 0にすれば簡単 *)
  | NonTail(x), FAddD(y, z) -> Printf.fprintf oc "\tadd.s\t%s, %s, %s\n" x y z
  | NonTail(x), FSubD(y, z) -> Printf.fprintf oc "\tsub.s\t%s, %s, %s\n" x y z
  | NonTail(x), FMulD(y, z) -> Printf.fprintf oc "\tmul.s\t%s, %s, %s\n" x y z
  | NonTail(x), FDivD(y, z) -> Printf.fprintf oc "\tdiv.s\t%s, %s, %s\n" x y z
  | NonTail(x), LdDF(y, V(z), i) -> 
    (if i = 1 then
	Printf.fprintf oc "\taddu\t$at, %s, $zero\n" z
     else if i = 4 then
       Printf.fprintf oc "\tsll\t$at, %s, 2\n" z
     else raise Shift_amount_is_not_4_error);
    Printf.fprintf oc "\taddu\t$at, %s, $at\n" y;
    Printf.fprintf oc "\tlw\t$at, 0($at)\n";
    Printf.fprintf oc "\tmtc1\t$at, %s\n" x
  | NonTail(x), LdDF(y, C(j), i) ->
    Printf.fprintf oc "\taddiu\t$at, %s, %d\n" y (j * i);
    Printf.fprintf oc "\tlw\t$at, 0($at)\n";
    Printf.fprintf oc "\tmtc1\t$at, %s\n" x
  | NonTail(_), StDF(x, y, V(z), i) -> 
    (if i = 1 then
    	Printf.fprintf oc "\taddu\t$fp, %s, $zero\n" z
     else if i = 4 then
       Printf.fprintf oc "\tsll\t$fp, %s, 2\n" z
     else raise Shift_amount_is_not_4_error);
    Printf.fprintf oc "\taddu, $fp, %s, $fp\n" y;
    Printf.fprintf oc "\tmfc1\t$at, %s\n" x;
    Printf.fprintf oc "\tsw\t$at, 0($fp)\n"
  | NonTail(_), StDF(x, y, C(j), i) ->
    Printf.fprintf oc "\tmfc1\t$at, %s\n" x;
    Printf.fprintf oc "\tsw\t$at, %d(%s)" (j * i) y
  | NonTail(_), Comment(s) -> Printf.fprintf oc "\t# %s\n" s
  (* 退避の仮想命令の実装 (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tsw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      Printf.fprintf oc "\tmfc1\t$at, %s\n" x;
      Printf.fprintf oc "\tsw\t$at, %d(%s)\n" (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tlw\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tlw\t$at, %d(%s)\n" (offset y) reg_sp;
      Printf.fprintf oc "\tmtc1\t$at, %s\n" x
  (* 末尾だったら計算結果を$v0にセットしてret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tjr\t$ra\n";
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
      g' oc (NonTail(reg_ret), exp);
      Printf.fprintf oc "\tjr\t$ra\n";
  | Tail, (FSet _ | FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
     g' oc (NonTail(fregs.(0)), exp);
     Printf.fprintf oc "\tjr\t$ra\n";
  | Tail, (Restore(x) as exp) ->
      (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp) (*?*)
      | _ -> assert false);
      Printf.fprintf oc "\tjr\t$ra\n";
  | Tail, IfEq(x, V(y), e1, e2) ->
      g'_tail_ifeq oc x y e1 e2
  | Tail, IfEq(x, C(y), e1, e2) ->
      Printf.fprintf oc "\taddiu\t$at, $zero, %d\n" y;
      g'_tail_ifeq oc x "$at" e1 e2
  | Tail, IfLE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tslt\t$at, %s, %s\n" y x;
      g'_tail_ifeq oc "$at" "$zero" e1 e2
  | Tail, IfLE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\taddiu\t$at, $zero, %d\n" y;
      Printf.fprintf oc "\tslt\t$at, $at, %s\n" x;
      g'_tail_ifeq oc "$at" "$zero" e1 e2
  | Tail, IfGE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tslt\t$at, %s, %s\n" x y;
      g'_tail_ifeq oc "$at" "$zero" e1 e2
  | Tail, IfGE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\taddiu\t$at, $zero, %d\n" y;
      Printf.fprintf oc "\tslt\t$at, %s, $at\n" x;
      g'_tail_ifeq oc "$at" "$zero" e1 e2
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.eq.s\t%s, %s\n" x y;
      g'_tail_if_ftrue oc e1 e2
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.ole.s\t%s, %s\n" x y;
      g'_tail_if_ftrue oc e1 e2
  | NonTail(z), IfEq(x, V(y), e1, e2) ->
      g'_non_tail_ifeq oc (NonTail(z)) x y e1 e2
  | NonTail(z), IfEq(x, C(y), e1, e2) ->
      Printf.fprintf oc "\taddiu\t$at, $zero, %d\n" y;
      g'_non_tail_ifeq oc (NonTail(z)) x "$at" e1 e2
  | NonTail(z), IfLE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tslt\t$at, %s, %s\n" y x;
      g'_non_tail_ifeq oc (NonTail(z)) "$at" "$zero" e1 e2
  | NonTail(z), IfLE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\taddiu\t$at, $zero, %d\n" y;
      Printf.fprintf oc "\tslt\t$at, $at, %s\n" x;
      g'_non_tail_ifeq oc (NonTail(z)) "$at" "$zero" e1 e2
  | NonTail(z), IfGE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tslt\t$at, %s, %s\n" x y;
      g'_non_tail_ifeq oc (NonTail(z)) "$at" "$zero" e1 e2
  | NonTail(z), IfGE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\taddiu\t$at, $zero, %d\n" y;
      Printf.fprintf oc "\tslt\t$at, %s, $at\n" x;
      g'_non_tail_ifeq oc (NonTail(z)) "$at" "$zero" e1 e2
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.eq.s\t%s, %s\n" x y;
      g'_non_tail_if_ftrue oc (NonTail(z)) e1 e2
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.ole.s\t%s, %s\n" x y;
      g'_non_tail_if_ftrue oc (NonTail(z)) e1 e2
  (* 関数呼び出しの仮想命令の実装 (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
      g'_args oc [(x, reg_cl)] ys zs; (* 引数のセット *)
      Printf.fprintf oc "\tj\t%s\n" x;
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tj\t%s\n" x;
  | NonTail(a), CallCls(x, ys, zs) ->
    raise Not_supported_yet
      (* g'_args oc [(x, reg_cl)] ys zs; *)
      (* let ss = stacksize () in *)
      (* if ss > 0 then Printf.fprintf oc "\taddiu\t%s, %d\n" reg_sp ss; *)
      (* Printf.fprintf oc "\tcall\t*(%s)\n" reg_cl; *)
      (* if ss > 0 then Printf.fprintf oc "\taddiu\t%s, %d\n" reg_sp (-1*ss); *)
      (* if List.mem a allregs && a <> regs.(0) then *)
      (*   Printf.fprintf oc "\taddu\t%s, %s\n" a regs.(0) *)
      (* else if List.mem a allfregs && a <> fregs.(0) then *)
      (*   Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a *)
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      if ss > 0 then
	(Printf.fprintf oc "\tsw\t$ra, %d($sp)\n" ss;
	Printf.fprintf oc "\taddiu\t%s, %s, %d\n" reg_sp reg_sp (-1*ss-4));
      Printf.fprintf oc "\tjal\t%s\n" x;
      if ss > 0 then
	(Printf.fprintf oc "\taddiu\t%s, %s, %d\n" reg_sp reg_sp (ss+4);
	 Printf.fprintf oc "\tlw\t$ra, %d($sp)\n" ss);
      if List.mem a allregs && a <> reg_ret then
        Printf.fprintf oc "\taddu\t%s, $v0, $zero\n" a 
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tmov.s\t%s, %s\n" a fregs.(0)
and g'_tail_ifeq oc x y e1 e2 =
  let label = Id.genid ("branch") in
  Printf.fprintf oc "\tbne\t%s, %s, %s\n" x y label;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" label;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_tail_if oc e1 e2 b bn = (* b has little mean, bn is instruction *)
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_tail_if_ftrue oc e1 e2 =
  let label = Id.genid ("branch") in
  Printf.fprintf oc "\tbc1t\t%s\n" label;
  let stackset_back = !stackset in
  g oc (Tail, e2);
  Printf.fprintf oc "%s:\n" label;
  stackset := stackset_back;
  g oc (Tail, e1)
and g'_non_tail_ifeq oc dest x y e1 e2 =
  let b_else = Id.genid "b_else" in
  let b_cont = Id.genid "b_cont" in
  Printf.fprintf oc "\tbne\t%s, %s, %s\n" x y b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tj\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_non_tail_if_ftrue oc dest e1 e2 =
  let b_true = Id.genid "b_true" in
  let b_cont = Id.genid "b_cont" in
  Printf.fprintf oc "\tbc1t\t%s\n" b_true;
  let stackset_back = !stackset in
  g oc (dest, e2);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tj\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_true;
  stackset := stackset_back;
  g oc (dest, e1);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\taddu\t%s, %s, $zero\n" r y)
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tmov.s\t%s, %s\n" z fr)
    (shuffle sw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  List.iter
    (fun (Id.L(x), d) ->
      Printf.fprintf oc "%s:\t# %f\n" x d;
      Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
      Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d))
    data;
  List.iter (fun fundef -> h oc fundef) fundefs;
  stackset := S.empty;
  stackmap := [];
  Printf.fprintf oc "main:\n";
  g oc (NonTail(regs.(0)), e);
