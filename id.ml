type t = string (* 変数の名前 (caml2html: id_t) *)
type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)

let print_id outchan id indent =
  let rec make_indent outchan indent =
    if indent > 0 
    then 
      (output_char outchan ' ';
       output_char outchan ' ';
       make_indent outchan (indent-1))
    else ()
  in 
  make_indent outchan indent;
  output_string outchan (id^"\n")

let rec print_id_list outchan idl indent =
  match idl with
    | [] -> ()
    | id :: res -> 
      (print_id outchan id indent;
       print_id_list outchan res indent)

let print_label outchan label indent =
  let rec make_indent outchan indent =
    if indent > 0 
    then 
      (output_char outchan ' ';
       output_char outchan ' ';
       make_indent outchan (indent-1))
    else ()
  in 
  match label with
    | L string ->
      (make_indent outchan indent;
       output_string outchan (string^"\n"))

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let rec id_of_typ = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Float -> "d"
  | Type.Fun _ -> "f"
  | Type.Tuple _ -> "t"
  | Type.Array _ -> "a" 
  | Type.Var _ -> assert false
let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_typ typ) !counter
