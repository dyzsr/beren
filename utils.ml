(* Unique identifier *)
type id = int

type idgen =
  { current_id : unit -> id
  ; new_id : unit -> id
  }

let make_idgen init =
  let id = ref init in
  let current_id () = !id in
  let new_id () = let x = !id in id := x + 1; x in
  { current_id; new_id }