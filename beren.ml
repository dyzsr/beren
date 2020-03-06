let token_to_string = 
  let open Parser in function
  | WILDCARD -> "_"
  | INT i -> string_of_int i
  | BOOL b -> string_of_bool b
  | CHAR c -> "'" ^ String.make 1 c ^ "'"
  | STRING s -> "\"" ^ s ^ "\""
  | IDENT id -> id
  | CAPID id -> id
  | TYPESYMBOL s -> "'" ^ s
  | EOF -> "EOF"
  | LET -> "let"
  | TYPE -> "type" 
  | METHOD -> "method"
  | AND -> "and"
  | REC -> "rec"
  | IN -> "in"
  | MUTABLE -> "mutable"
  | OF -> "of"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | MATCH -> "match"
  | WITH -> "with"
  | INTERFACE -> "interface"
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
  | ASSIGNFIELD -> "<-"

let lexer lexbuf =
  let tok = Lexer.lex lexbuf in
  let () = print_endline ("token: " ^ token_to_string tok) in
  tok

let _ = 
  match Array.length Sys.argv with
  | 1 -> failwith "no input file"
  | n ->
    let file = Sys.argv.(1) in
    let () = print_endline file in
    let chan = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel chan in
    let rec aux = function
    | Parser.EOF -> ()
    | v -> print_endline (token_to_string v); aux (Lexer.lex lexbuf)
    in aux (Lexer.lex lexbuf)
    (* try
      let result = Parser.program lexer lexbuf in
      print_newline (); flush stdout
    with Parser.Error ->
      let pos_to_string Lexing.{pos_lnum=lnum; pos_cnum=cnum} =
        let lstr = "line " ^ string_of_int lnum in
        let cstr = "col " ^ string_of_int cnum in
        "[" ^ lstr ^ ", " ^ cstr ^ "]"
      in
      let start_p = Lexing.lexeme_start_p lexbuf in
      let end_p = Lexing.lexeme_end_p lexbuf in
      print_endline ("Syntax error between "
        ^ pos_to_string start_p ^ " and " ^ pos_to_string end_p) *)