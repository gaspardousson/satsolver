open Translater;;
open Satsolver;;

let () =
    let path = Sys.argv.(1) in
    print_endline (string_of_bool (naive_solver(read_cnf(path))).(0))
;;