{
open Parser
exception Invalid_lexeme of string

let string_of_chars list =
  String.concat "" (List.map (String.make 1) list)
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alnum = digit | alpha | '_'

rule lex = parse
    space       { lex lexbuf }
  | "true"      { BOOL true }
  | "false"     { BOOL false }
  | digit+ as lxm { INT (int_of_string lxm) }
  | "_"         { WILDCARD }
  | lowercase alnum* as lxm { IDENT lxm }
  | uppercase alnum* as lxm { CAPID lxm }
  | "'"         { lex_quote lexbuf }
  | "#'"        { lex_char [] lexbuf }
  | "\""        { lex_string [] lexbuf }
  | "let"       { LET }
  | "type"      { TYPE }
  | "method"    { METHOD }
  | "and"       { AND }
  | "rec"       { REC }
  | "in"        { IN }
  | "mutable"   { MUTABLE }
  | "of"        { OF }
  | "fun"       { FUN }
  | "function"  { FUNCTION }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "match"     { MATCH }
  | "with"      { WITH }
  | "interface" { INTERFACE }
  | "end"       { END }
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
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "[|"  { LBRACKBAR }
  | "|]"  { RBRACKBAR }
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
  | "<-"  { ASSIGNFIELD }
  | "(*"  { lex_comment lexbuf; lex lexbuf }
  | eof   { EOF }

and lex_comment = parse
    "*)" {}
  | "(*" { lex_comment lexbuf; lex_comment lexbuf }
  | _    { lex_comment lexbuf }

and lex_quote = parse
    alpha alnum* as lxm { TYPESYMBOL lxm }

and lex_char acc = parse
    "\'" {
      match acc with
      | [x] -> CHAR x
      | _ -> raise (Invalid_lexeme ("'" ^ string_of_chars (List.rev acc) ^ "'"))
    }
  | "\\n" { lex_char ('\n'::acc) lexbuf }
  | "\\r" { lex_char ('\r'::acc) lexbuf }
  | "\\t" { lex_char ('\t'::acc) lexbuf }
  | "\\\"" { lex_char ('\"'::acc) lexbuf }
  | "\\\'" { lex_char ('\''::acc) lexbuf }
  | "\\\\" { lex_char ('\\'::acc) lexbuf }
  | _ as c { lex_char (c :: acc) lexbuf }

and lex_string acc = parse
    "\"" { STRING (string_of_chars (List.rev acc)) }
  | "\\n" { lex_string ('\n'::acc) lexbuf }
  | "\\r" { lex_string ('\r'::acc) lexbuf }
  | "\\t" { lex_string ('\t'::acc) lexbuf }
  | "\\\"" { lex_string ('\"'::acc) lexbuf }
  | "\\\'" { lex_string ('\''::acc) lexbuf }
  | "\\\\" { lex_string ('\\'::acc) lexbuf }
  | _ as c { lex_string (c :: acc) lexbuf }