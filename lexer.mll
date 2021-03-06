{
  open Parser
  exception Invalid_lexeme of string
}

let eol = '\n'
let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alnum = digit | alpha | '_'

rule lex = parse
    eol         { Lexing.new_line lexbuf; lex lexbuf }
  | space       { lex lexbuf }
  | "true"      { BOOL true }
  | "false"     { BOOL false }
  | "'"         { lex_quote lexbuf }
  | "#'"        { lex_char [] lexbuf }
  | "\""        { lex_string [] lexbuf }
  | "let"       { LET }
  | "type"      { TYPE }
  | "and"       { AND }
  | "rec"       { REC }
  | "in"        { IN }
  | "of"        { OF }
  | "fun"       { FUN }
  | "function"  { FUNCTION }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "match"     { MATCH }
  | "with"      { WITH }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { TIMES }
  | "/"   { DIV }
  | "%"   { MOD }
  | "^"   { CONCAT }
  | "<"   { LT }
  | "<="  { LTE }
  | ">"   { GT }
  | ">="  { GTE }
  | "="   { EQ }
  | "!="  { NEQ }
  | "&&"  { LOGIC_AND }
  | "||"  { LOGIC_OR }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "["   { LBRACK }
  | "]"   { RBRACK }
  | "|"   { BAR }
  | "@"   { APPEND }
  | "::"  { CONS }
  | "!"   { DEREF }
  | "=>"  { TO }
  | "->"  { ARROW }
  | ";"   { SEMICOLON }
  | ":"   { COLON }
  | ","   { COMMA }
  | "."   { PERIOD }
  | ":="  { ASSIGN }
  | "(*"  { lex_comment lexbuf; lex lexbuf }
  | digit+ as lxm { INT (int_of_string lxm) }
  | ("_" | lowercase) alnum* as lxm { IDENT lxm }
  | uppercase alnum* as lxm         { CAPID lxm }
  | "_"   { WILDCARD }
  | eof   { EOF }

and lex_comment = parse
    "*)" {}
  | "(*" { lex_comment lexbuf; lex_comment lexbuf }
  | eol  { Lexing.new_line lexbuf; lex_comment lexbuf }
  | _    { lex_comment lexbuf }

and lex_quote = parse
    eol   { Lexing.new_line lexbuf; lex_quote lexbuf }
  | space { lex_quote lexbuf }
  | alpha alnum* as lxm { TYPEVAR lxm }

and lex_char acc = parse
    "\'" {
      match acc with
      | [x] -> CHAR x
      | _ -> raise (Invalid_lexeme ("'" ^ Utils.string_of_chars (List.rev acc) ^ "'"))
    }
  | "\\n" { lex_char ('\n'::acc) lexbuf }
  | "\\r" { lex_char ('\r'::acc) lexbuf }
  | "\\t" { lex_char ('\t'::acc) lexbuf }
  | "\\\"" { lex_char ('\"'::acc) lexbuf }
  | "\\\'" { lex_char ('\''::acc) lexbuf }
  | "\\\\" { lex_char ('\\'::acc) lexbuf }
  | eol    { Lexing.new_line lexbuf; lex_char ('\n'::acc) lexbuf }
  | _ as c { lex_char (c :: acc) lexbuf }

and lex_string acc = parse
    "\"" { STRING (Utils.string_of_chars (List.rev acc)) }
  | "\\n" { lex_string ('\n'::acc) lexbuf }
  | "\\r" { lex_string ('\r'::acc) lexbuf }
  | "\\t" { lex_string ('\t'::acc) lexbuf }
  | "\\\"" { lex_string ('\"'::acc) lexbuf }
  | "\\\'" { lex_string ('\''::acc) lexbuf }
  | "\\\\" { lex_string ('\\'::acc) lexbuf }
  | eol    { Lexing.new_line lexbuf; lex_string ('\n'::acc) lexbuf }
  | _ as c { lex_string (c :: acc) lexbuf }
