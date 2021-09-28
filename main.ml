open Translater;;
open Satsolver;;

let bool path n =
    let prob = Array.make n [||] in
    for i = 1 to n do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    for i = 0 to n-1 do
        print_endline (string_of_bool (naive_solver (prob.(i))))
    done;;

let time path n =
    let prob = Array.make n [||] in
    for i = 1 to n do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    let t0 = Sys.time () in
    for i = 0 to n-1 do
        ignore (naive_solver (prob.(i)))
    done;
    let t1 = Sys.time () in
    print_endline (string_of_float ((t1-.t0)/.(float_of_int n)));;

let () =
    let path = Sys.argv.(1) in
    let n = int_of_string Sys.argv.(2) in
    let quantity = Sys.argv.(3) in
    match quantity with
        |"bool" -> bool path n
        |"time" -> time path n
        |_ -> failwith "Invalid quantity"
;;