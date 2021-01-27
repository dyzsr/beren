module Name_map = Map.Make (String)
module Id_map = Map.Make (Int)

(* Unique identifier *)
type id = int

type idgen = { current_id : unit -> id; new_id : unit -> id }

let make_idgen init =
  let id = ref init in
  let current_id () = !id in
  let new_id () =
    let x = !id in
    id := x + 1;
    x
  in
  { current_id; new_id }
;;

(* Lexing *)
let pos_to_string Lexing.{ pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum } =
  let lstr = "line " ^ string_of_int lnum in
  let cstr = "column " ^ string_of_int (cnum - bol) in
  lstr ^ ", " ^ cstr
;;

let string_of_chars list = String.concat "" (List.map (String.make 1) list)
