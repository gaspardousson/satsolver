open Translater;;
open Satsolver;;

let f = plf_of_string("(1 ^ -(-1 v 2))");;
print_endline (string_of_plf f);;