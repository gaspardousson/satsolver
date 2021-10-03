open Translater;;
open Satsolver;;

let solver = naive_solver;;

let bool path n =
    let prob = Array.make n [||] in
    for i = 1 to n do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    for i = 0 to n-1 do
        if solver (prob.(i))
            then print_endline "sat"
            else print_endline "unsat"
    done
;;

let time path n =
    let prob = Array.make n [||] in
    for i = 1 to n do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    let t0 = Sys.time () in
    for i = 0 to n-1 do
        ignore (solver (prob.(i)))
    done;
    let t1 = Sys.time () in
    print_endline (string_of_float ((t1-.t0)/.(float_of_int n)))
;;

let () =
    if Array.length Sys.argv = 4 then
    begin
        let path = Sys.argv.(1) in
        let n = int_of_string Sys.argv.(2) in
        let quantity = Sys.argv.(3) in
        match quantity with
            |"bool" -> bool path n
            |"time" -> time path n
            |_ -> failwith "Invalid quantity";
    end
;;