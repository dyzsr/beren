let token_to_string = 
  let open Parser in function
  | WILDCARD -> "_"
  | INT i -> string_of_int i
  | BOOL b -> string_of_bool b
  | CHAR c -> "'" ^ String.make 1 c ^ "'"
  | STRING s -> "\"" ^ s ^ "\""
  | IDENT id -> "id<" ^ id ^ ">"
  | CAPID id -> "id<" ^ id ^ ">"
  | TYPEVAR s -> "'" ^ s
  | EOF -> "EOF"
  | LET -> "let"
  | TYPE -> "type" 
  | METHOD -> "method"
  | AND -> "and"
  | REC -> "rec"
  | IN -> "in"
  | OF -> "of"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | MATCH -> "match"
  | WITH -> "with"
  | SIG -> "sig"
  | END -> "end"
  | PLUS -> "+" 
  | MINUS -> "-" 
  | TIMES -> "*" 
  | DIV -> "/" 
  | MOD -> "%" 
  | CONCAT -> "^"
  | LT -> "<" 
  | LTE -> "<=" 
  | GT -> ">" 
  | GTE -> ">=" 
  | EQ -> "=" 
  | NEQ -> "!="
  | LOGIC_AND -> "&&" 
  | LOGIC_OR -> "||"
  | LPAREN -> "(" 
  | RPAREN -> ")" 
  | LBRACK -> "[" 
  | RBRACK -> "]" 
  | LBRACE -> "{" 
  | RBRACE -> "}"
  | LBRACKBAR -> "[|" 
  | RBRACKBAR -> "|]"
  | BAR -> "|" 
  | APPEND -> "@" 
  | CONS -> "::" 
  | DEREF -> "!"
  | TO -> "=>" 
  | ARROW -> "->"
  | SEMICOLON -> ";" 
  | COLON -> ":" 
  | COMMA -> "," 
  | PERIOD -> "."
  | ASSIGN -> ":=" 

let lexer lexbuf =
  let tok = Lexer.lex lexbuf in
  (* let () = print_endline ("token: " ^ token_to_string tok) in *)
  tok

let read_token_until_eof lexer lexbuf =
  let rec aux = function
  | Parser.EOF -> ()
  | v -> aux (lexer lexbuf)
  in aux (lexer lexbuf)

exception Syntax_error of string

let parse lexer lexbuf = 
  try
    Parser.main lexer lexbuf
  with Parser.Error ->
    let token = Lexing.lexeme lexbuf in
    let start_p  = Utils.pos_to_string (Lexing.lexeme_start_p lexbuf) in
    let error_string = start_p ^ ": token '" ^ token ^ "'" in
    raise (Syntax_error error_string)


exception Invalid_filename of string

let split_extension filename =
  match List.rev (String.split_on_char '.' filename) with
  | [] -> raise (Invalid_filename filename)
  | [x] -> raise (Invalid_filename filename)
  | ext :: names ->
    if ext <> "brn" then raise (Invalid_filename filename) else
    let name = String.concat "." (List.rev names) in
    name, ext

let _ = 
  let _ = match Array.length Sys.argv with
    | 1 -> failwith "no input file"
    | n -> n
  in

  let file = Sys.argv.(1) in
  let name, ext = split_extension file in
  (* let () = print_endline file in *)
  let in_chan = open_in file in

  let lexbuf = Lexing.from_channel in_chan in
  (* read_token_until_eof lexer lexbuf *)
  let result = parse lexer lexbuf in
  let () = close_in in_chan in

  let print_ast decl = print_endline (Ast.rep_to_string (Ast.decl_to_rep decl)) in
  let () = List.iter print_ast result in
  let symtab, asts = Type_inference.walk_decl_list result in

  let () = Types.print_symtab symtab in
  let code = Codegen.walk_decl_list asts in
  let text = Vm.text_program code in

  let out_chan =
    let filename = name ^ ".brc" in
    open_out filename
  in
  let () = output_string out_chan text in
  close_out out_chan