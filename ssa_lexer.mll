{
  open Ssa_parser
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
    eol           { Lexing.new_line lexbuf; EOL }
  | space         { lex lexbuf }
  | eof           { EOF }
  | ".external"   { EXTERNAL }
  | ".global"     { GLOBAL }
  | ".captured"   { CAPTURED }
  | ".numvars"    { NUMVARS }
  | ".func"       { FUNC }
  | ".entry"      { ENTRY }
  | ".body"       { BODY }
  | "_"           { WILDCARD }
  | "("           { LPAR }
  | ")"           { RPAR }
  | ","           { COMMA }
  | "#'"          { lex_char [] lexbuf }
  | "\""          { lex_string [] lexbuf }
  | "true" | "false" as lxm     { BOOL (bool_of_string lxm) }
  | ("+" | "-")? digit+ as lxm  { INT (int_of_string lxm) }
  | ("%" | "_" | alpha) alnum* as lxm { ID lxm }
  | "." alpha alnum* as lxm     { LABEL lxm }

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
    "\"" { STR (Utils.string_of_chars (List.rev acc)) }
  | "\\n" { lex_string ('\n'::acc) lexbuf }
  | "\\r" { lex_string ('\r'::acc) lexbuf }
  | "\\t" { lex_string ('\t'::acc) lexbuf }
  | "\\\"" { lex_string ('\"'::acc) lexbuf }
  | "\\\'" { lex_string ('\''::acc) lexbuf }
  | "\\\\" { lex_string ('\\'::acc) lexbuf }
  | eol    { Lexing.new_line lexbuf; lex_string ('\n'::acc) lexbuf }
  | _ as c { lex_string (c :: acc) lexbuf }