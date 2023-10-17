{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-' ? digit+

rule read = 
  parse
  | white { read lexbuf }
  | int {INT (int_of_string (Lexing.lexeme lexbuf))}
  | "+" {PLUS }
  | "*" {TIMES}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }

