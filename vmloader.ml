open Vm

let load filename =
  let chan = open_in filename in
  input_line chan