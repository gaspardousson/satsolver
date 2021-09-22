{

open Parser

exception SyntaxError of string

}

let int = ['0'-'9']+
let not = ['-' '!']
let conj = ['^' '&']
let disj = ['v' '|']
let whitespace = [' ' '\t' '\n']
let comment = '#' [^'\n']* '\n'

rule read_token =
    parse
    |int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    |not { NOT }
    |conj { CONJ }
    |disj { DISJ }
    |'(' { LPAR }
    |')' { RPAR }
    |whitespace { read_token lexbuf }
    |comment { read_token lexbuf }
    |eof { EOF }
    |_ { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
