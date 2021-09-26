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


let read_cnf path =
    let rec read_clause list depth =
        match list with
            |[] -> Array.make depth 0
            |["0"] -> Array.make depth 0
            |""::q -> read_clause q depth
            |t::q -> let clause = read_clause q (depth+1) in
                        clause.(depth) <- int_of_string t;
                        clause
    in
    let rec read_line file depth =
        let line = input_line file in
        match line.[0] with
            |'c' -> read_line file depth
            |'p' -> read_line file depth
            |'%' -> Array.make depth [||]
            |_ -> let cnf = read_line file (depth+1) in
                    cnf.(depth) <- read_clause (String.split_on_char ' ' line) 0;
                    cnf
    in
    read_line (open_in path) 0
;;