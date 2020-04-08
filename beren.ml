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

let pos_to_string Lexing.{pos_lnum=lnum; pos_bol=bol; pos_cnum=cnum} =
  let lstr = "line " ^ string_of_int lnum in
  let cstr = "column " ^ string_of_int (cnum - bol) in
  lstr ^ ", " ^ cstr

let read_token_until_eof lexer lexbuf =
  let rec aux = function
  | Parser.EOF -> ()
  | v -> aux (lexer lexbuf)
  in aux (lexer lexbuf)

exception SyntaxError of string

let parse lexer lexbuf = 
  try
    Parser.main lexer lexbuf
  with Parser.Error ->
    let token = Lexing.lexeme lexbuf in
    let start_p  = pos_to_string (Lexing.lexeme_start_p lexbuf) in
    let error_string = start_p ^ ": Syntax error at token '" ^ token ^ "'" in
    raise (SyntaxError error_string)

let _ = 
  match Array.length Sys.argv with
  | 1 -> failwith "no input file"
  | n ->
    let file = Sys.argv.(1) in
    (* let () = print_endline file in *)
    let chan = open_in file in
    let lexbuf = Lexing.from_channel chan in
    (* read_token_until_eof lexer lexbuf *)
    try
      let result = parse lexer lexbuf in
      let print_ast decl = print_endline (Ast.rep_to_string (Ast.decl_to_rep decl)) in
      let () = List.iter print_ast result in
      let symtab = Types.walk_decl_list result in
      Types.print_symtab symtab
    with SyntaxError s -> print_endline s