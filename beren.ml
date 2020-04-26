let lexer lexbuf =
  try
    Ssa_lexer.lex lexbuf
  with Failure s ->
    let token = Lexing.lexeme lexbuf in
    let start_p  = Utils.pos_to_string (Lexing.lexeme_start_p lexbuf) in
    let error_string = start_p ^ ": Syntax error at token '" ^ token ^ "'" in
    failwith (s ^ "; " ^ error_string)

exception Invalid_ssa_form of string

let parse lexer lexbuf =
  try
    Ssa_parser.program lexer lexbuf
  with Ssa_parser.Error ->
    let token = Lexing.lexeme lexbuf in
    let start_p  = Utils.pos_to_string (Lexing.lexeme_start_p lexbuf) in
    let error_string = start_p ^ ": Syntax error at token '" ^ token ^ "'" in
    raise (Invalid_ssa_form error_string)

let () =
  let _ = match Array.length Sys.argv with
    | 1 -> failwith "no input file"
    | n -> n
  in
  let file = Sys.argv.(1) in
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let program = parse lexer lexbuf in
  let machine = Vm.create program in 
  while machine.state do
    (* print_endline (string_of_bool machine.state); *)
    Vm.run machine
  done
  