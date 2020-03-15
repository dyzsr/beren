open Ast

let check_decl = function
  | TypeBinding b -> Ok ()
  | ValueBinding b -> Ok ()
  | MethodBinding b -> Ok ()