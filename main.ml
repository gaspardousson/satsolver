open Translater;;
open Satsolver;;



let solver = cdcl_solver;;


let mu a =
    let n = Array.length a in
    let m = ref 0. in
    for i = 0 to n - 1 do
        m := !m +. a.(i)
    done;
    !m /. float_of_int n
;;

let sigma a =
    let n = Array.length a in
    let s = ref 0. in
    let m = mu a in
    for i = 0 to n - 1 do
        s := !s +. (m -. a.(i)) ** 2.
    done;
    sqrt (!s /. float_of_int (n-1))
;;


let bool path n_pb =
    let prob = Array.make n_pb (0, 0, [||]) in
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
    let prob = Array.make n_pb (0, 0, [||]) in
    for i = 1 to n_pb do
        prob.(i-1) <- read_cnf(path ^ (string_of_int i) ^ ".cnf")
    done;
    let t_pb = Array.make n_pb 0. in
    for i = 1 to n_pb do
        let t0 = Sys.time () in
        ignore (solver prob.(i-1));
        let t1 = Sys.time () in
        t_pb.(i-1) <- t1 -. t0
    done;
    let n_var, n_cla, cnf = prob.(0) in
    print_endline (string_of_int n_var ^ "-" ^
                   string_of_int n_cla ^ "-" ^
                   string_of_float (1000. *. mu t_pb) ^ "-" ^
                   string_of_float (1000. *. sigma t_pb /. sqrt(float_of_int n_pb)))
;;


let () =
    match Array.length Sys.argv with
    |1 -> failwith "No work provided"
    |2 -> if solver (read_cnf(Sys.argv.(1)))
            then print_endline "sat"
            else print_endline "unsat"
    |3 -> let path = Sys.argv.(1) in
          let n_pb = int_of_string Sys.argv.(2) in
          bool path n_pb
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