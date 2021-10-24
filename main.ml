open Translater;;
open Satsolver;;

let solver = quine_solver;;

let bool path n_pb =
    let prob = Array.make n_pb (0, [||]) in
    for i = 1 to n_pb do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    for i = 1 to n_pb do
        if solver prob.(i-1)
            then print_endline "sat"
            else print_endline "unsat"
    done
;;

let time path n_pb =
    let prob = Array.make n_pb (0, [||]) in
    for i = 1 to n_pb do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    let t0 = Sys.time () in
    for i = 1 to n_pb do
        ignore (solver prob.(i-1))
    done;
    let t1 = Sys.time () in
    print_endline (string_of_int (fst prob.(0)) ^ "-" ^ string_of_float ((t1-.t0)*.1000./.(float_of_int n_pb)))
;;

let () =
    match Array.length Sys.argv with
    |1 -> failwith "No work provided"
    |2 -> if solver (read_cnf(Sys.argv.(1)))
            then print_endline "sat"
            else print_endline "unsat"
    |4 -> begin
            let path = Sys.argv.(1) in
            let n_pb = int_of_string Sys.argv.(2) in
            let quantity = Sys.argv.(3) in
            match quantity with
                |"bool" -> bool path n_pb
                |"time" -> time path n_pb
                |_ -> failwith "Invalid quantity"
            end
    |_ -> failwith ("Unrecognized number of arguments (" ^ (string_of_int (Array.length Sys.argv)) ^ ")")
;;