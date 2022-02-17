open Reader;;
open Auxiliary;;
open Satsolver;;


(* Choix du solveur utilisé par la suite *)
let solveur = solveur_cdcl;;


let mesurer_sat chemin n_pb =
(* Résout les problèmes SAT fournis en entrée. *)
    let prob = Array.make n_pb (0, 0, [||]) in
    for i = 1 to n_pb do
        prob.(i-1) <- lire_cnf (chemin ^ (string_of_int i) ^ ".cnf")
    done;
    for i = 1 to n_pb do
        if solveur prob.(i-1)
            then print_endline "sat"
            else print_endline "unsat"
    done
;;


let mesurer_temps chemin n_pb =
(* Mesure le temps de résolution des problèmes SAT fournis en entrée. *)
    let prob = Array.make n_pb (0, 0, [||]) in
    for i = 1 to n_pb do
        prob.(i-1) <- lire_cnf (chemin ^ (string_of_int i) ^ ".cnf")
    done;
    let t_pb = Array.make n_pb 0. in
    for i = 1 to n_pb do
        let t0 = Sys.time () in
        ignore (solveur prob.(i-1));
        let t1 = Sys.time () in
        t_pb.(i-1) <- t1 -. t0
    done;
    let n_var, n_clauses, cnf = prob.(0) in
    print_endline (string_of_int n_var ^ "-" ^
                   string_of_int n_clauses ^ "-" ^
                   string_of_float (1000. *. moyenne t_pb) ^ "-" ^
                   string_of_float (1000. *. ecart_type t_pb /. sqrt(float_of_int n_pb)))
;;


let () =
(*
    Format d'argument(s) possible(s) :
        chemin -> Résout le problème indiqué par le chemin
        chemin, n_pb -> Résout les n_pb problèmes indiqués par le chemin
        chemin, n_pb, grandeur -> Résout les n_pb problèmes indiqués par le chemin et renvoie le résultat (si bool) ou  le temps de résolution (si time)
*)
    match Array.length Sys.argv with
    |1 -> failwith "Aucun travail donné"
    |2 -> if solveur (lire_cnf Sys.argv.(1))
            then print_endline "sat"
            else print_endline "unsat"
    |3 -> let chemin = Sys.argv.(1) in
          let n_pb = int_of_string Sys.argv.(2) in
          mesurer_sat chemin n_pb
    |4 -> begin
            let chemin = Sys.argv.(1) in
            let n_pb = int_of_string Sys.argv.(2) in
            let grandeur = Sys.argv.(3) in
            match grandeur with
                |"bool" -> mesurer_sat chemin n_pb
                |"time" -> mesurer_temps chemin n_pb
                |_ -> failwith "Grandeur invalide"
            end
    |_ -> failwith ("Nombre d'arguments incorrect (" ^ (string_of_int (Array.length Sys.argv)) ^ ")")
;;