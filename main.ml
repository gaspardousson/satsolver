open Translater;;
open Satsolver;;

let f = [| [| -1 |]; [| 1 ; 2 |] |];;
print_endline (string_of_bool (naive_solver f).(0));;