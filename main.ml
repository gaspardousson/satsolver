open Translater;;
open Satsolver;;

let f = [| [| -1 |]; [| 1 ; 2 |] |];;
print_endline (string_of_bool (naive_solver (read_cnf "test/UF020.91/uf20-01.cnf")).(0));;