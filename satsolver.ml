open Auxiliary

(* Naive solver *)

let naive_solver problem =
    let n_atoms, n_laws, cnf = problem in

    let rec main todo asgn =
        let asgn = Array.copy asgn in
        let atom = branch n_atoms asgn in
        if atom > n_atoms
            then not (check_conflict cnf asgn)
            else let ion = ionize atom in
                (asgn.(atom) <- ion; main [atom] asgn) ||
                (asgn.(atom) <- -ion; main [atom] asgn)
    in

    let asgn = Array.make (n_atoms + 1) 0 in
    main [] asgn
;;



(* Quine solver *)

let quine_solver problem =
    let n_atoms, n_laws, cnf = problem in

    let rec main todo asgn =
        let asgn = Array.copy asgn in
        if check_conflict cnf asgn
            then false
            else begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then true
                    else let ion = ionize atom in
                    	(asgn.(atom) <- ion; main [atom] asgn) ||
                        (asgn.(atom) <- -ion; main [atom] asgn)
            end
    in

    let asgn = Array.make (n_atoms + 1) 0 in
    main [] asgn
;;



(* DPLL solver *)

let dpll_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let asgn, watching = init_watching n_atoms cnf in

    let rec main todo asgn =
        let asgn = Array.copy asgn in
        if propagate todo watching cnf asgn
            then begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then true
                    else let ion = ionize atom in
                    	(asgn.(atom) <- ion; main [atom] asgn) ||
                        (asgn.(atom) <- -ion; main [atom] asgn)
            end
            else false
    in
    
    main [] asgn
;;



(* TODO: CDCL solver *)

let cdcl_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let asgn, watching = init_watching n_atoms cnf in
    let knwldg = Array.make 1 [] in
    ()
;;
