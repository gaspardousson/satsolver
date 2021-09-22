open Language;;
open Parser;;

let rec string_of_plf f = match f with
        |Var i -> string_of_int i
        |Not f -> "-" ^ (string_of_plf f)
        |And (f1, f2) -> "(" ^ (string_of_plf f1) ^ " ^ " ^ (string_of_plf f2) ^ ")"
        |Or (f1, f2) -> "(" ^ (string_of_plf f1) ^ " v " ^ (string_of_plf f2) ^ ")"
;;

let plf_of_string s =
    Parser.main Lexer.read_token (Lexing.from_string s)
;;