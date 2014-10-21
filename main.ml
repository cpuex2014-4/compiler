let limit = ref 1000
let print_syntax = ref false
let print_kNormal = ref false
let print_alpha = ref false

let rec iter n e = (* 最適化処理を繰り返す (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

let parse l =
  let res = (Parser.exp Lexer.token l) in
  if !print_syntax
  then 
    (let oc = open_out "Syntax.t" in
     Syntax.print_syntax oc res 0;
     close_out oc;
     res)
  else 
    res

let kNormalize l = 
  let res = (KNormal.f l) in
  if !print_kNormal
  then 
    (let oc = open_out "kNormal.t" in
     KNormal.print_kNorm oc res 0;
     close_out oc;
     res)
  else
    res

let alpha l = 
  let res = Alpha.f l in
  if !print_alpha
  then 
    (let oc = open_out "Alpha.t" in
     KNormal.print_kNorm oc res 0;
     close_out oc;
     res)
  else
    res

let lexbuf outchan l = (* バッファをコンパイルしてチャンネルに出力する(caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  let parsed = parse l in
  let typed = Typing.f parsed in
  let kNormalized = kNormalize typed in
  let alphaed = alpha kNormalized in
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
	  (Virtual.f
	     (Closure.f
		(iter !limit
		   alphaed)))))


let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に出力する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイルが開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated"); 
    ("-psyntax", Arg.Unit(fun () -> print_syntax := true), "print the parse result to Syntax.t");
    ("-pknorm", Arg.Unit(fun () -> print_kNormal := true), "print the kNormalize result to kNormal.t");
    ("-palpha", Arg.Unit(fun () -> print_alpha := true), "print the alpha convert result to Alpha.t")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
