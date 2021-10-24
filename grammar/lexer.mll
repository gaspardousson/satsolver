{

open Parser

exception SyntaxError of string

}

let int = '-'? ['0'-'9']+
let whitespace = [' ' '\t' '\n']
let comment = 'c' [^'\n']* '\n'

rule read_token =
    parse
    |"p cnf" { PB }
    |'0' { EOC }
    |'%' { EOCNF }
    |int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    |whitespace { read_token lexbuf }
    |comment { read_token lexbuf }
    |eof { EOF }
    |_ { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
