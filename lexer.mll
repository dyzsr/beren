{
  open Token
  exception Eof
}

let space = [' ' '\t' '\n' '\r' '\v']
let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']
let alnum = digit | alpha

rule lex = parse
    space    { lex lexbuf }
  | digit as lxm         { NUMBER (float_of_string lxm) }
  | alpha alnum+ as lxm  { IDENT lxm }
  | "def"    { DEF }
  | "extern" { EXTERN }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '<'      { LT }
  | '#'      { lex_comment lexbuf; lex lexbuf }
  | eof      { raise Eof }

rule lex_comment = parse
    '\n' {}
  | _    { lex_comment lexbuf }