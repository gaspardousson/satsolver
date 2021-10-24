(* Useful libraries *)

open Int;;
open Array;;



(* Naive solver *)

let naive_solver problem =
    let n_var, formula = problem in

    let rec power_two n =
        match n with
            |0 -> 1
            |1 -> 2
            |_ when n mod 2 = 0 -> let x = power_two (n/2) in x * x
            |_ -> let x = power_two (n/2) in 2 * x * x
    in

    let rec disj clause assignment =
        match clause with
            |[] -> false
            |var::queue when var > 0 -> ((shift_right assignment (var - 1)) land 1 = 1) || disj queue assignment
            |var::queue -> ((shift_right assignment (- var - 1)) land 1 = 0) || disj queue assignment
    in

    let conj cnf assignment =
        let satisfied = ref true in
        for i = 0 to length cnf - 1 do
            satisfied := !satisfied && disj cnf.(i) assignment
        done; !satisfied
    in

    let rec main formula assignment =
        match assignment with
            |0 -> conj formula assignment
            |_ -> conj formula assignment || main formula (pred assignment)

    in

    main formula (power_two n_var)
;;



(* Quine solver *)

let quine_solver problem =
    let n_var, formula = problem in
    let n = length formula in
    let satisfied = make n 0 in

    let rec is_conflictual clause i literal =
        match clause with
            |[] -> true
            |var::queue when var = literal -> satisfied.(i) <- abs var; false
            |var::queue -> (is_conflictual queue i literal) && (abs var <= abs literal)
    in

    let rec exists_conflict i literal =
        match i with
            |0 -> (satisfied.(i) = 0 && is_conflictual formula.(i) i literal)
            |_ -> exists_conflict (pred i) literal || (satisfied.(i) = 0 && is_conflictual formula.(i) i literal)
    in

    let rec main var_left literal =
        match var_left with
            |0 -> true
            |_ -> let conflict = exists_conflict (n - 1) literal in
                    if conflict
                        then begin
                            for i = 0 to n - 1 do
                                if satisfied.(i) = abs literal
                                then satisfied.(i) <- 0
                            done; false
                        end
                        else let next_lit = abs literal + 1
                             in main (var_left - 1) next_lit || main (var_left - 1) (-next_lit)
    in

    main n_var 0
;;



(* TODO: DPLL solver *)

let dpll_solver formula n_var =

    (* TODO: Two watched literal *)
    let twl = make 0 (1, 2) in

    (* TODO: Unit propagation *)
    let propagate () =
        ()
    in

    (* TODO: main *)
    let main () =
        ()
    in

    main ()
;;



(* TODO: CDCL solver *)

let cdcl_solver formula n_var =
    ()
;;
