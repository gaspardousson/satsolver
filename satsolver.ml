open Auxiliary

(* Naive solver *)

let naive_solver problem =
    let n_atoms, n_laws, cnf = problem in

    let rec main level todo asgn =
        let asgn = Array.copy asgn in
        let atom = branch n_atoms asgn in
        if atom > n_atoms
            then check_conflict cnf asgn
            else let ion = ionize atom in
                (asgn.(atom) <- ion; main (level+1) [atom] asgn) ||
                (asgn.(atom) <- -ion; main (level+1) [atom] asgn)
    in

    let asgn = Array.make (n_atoms+1) 0 in
    main 0 [] asgn
;;



(* Quine solver *)

let quine_solver problem =
    let n_atoms, n_laws, cnf = problem in

    let rec main level todo asgn =
        let asgn = Array.copy asgn in
        if check_conflict cnf asgn
            then begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then true
                    else let ion = ionize atom in
                         (asgn.(atom) <- ion; main (level+1) [atom] asgn) ||
                         (asgn.(atom) <- -ion; main (level+1) [atom] asgn)
                end else false
    in

    let asgn = Array.make (n_atoms+1) 0 in
    main 0 [] asgn
;;



(* DPLL solver *)

let dpll_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let watching = init_watching n_atoms cnf in

    let rec main level todo asgn =
        let asgn = Array.copy asgn in
        if propagate todo watching cnf asgn
            then begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then true
                    else let ion = ionize atom in
                    	(asgn.(atom) <- ion; main (level+1) [atom] asgn) ||
                        (asgn.(atom) <- -ion; main (level+1) [atom] asgn)
            end else false
    in

    let asgn = Array.make (n_atoms+1) 0 in
    main 0 [] asgn
;;



(* TODO: CDCL solver *)

let cdcl_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let watching = init_watching n_atoms cnf in

    let cnf = extend cnf n_laws 1000000 in
    let memory, n_clauses = Array.length cnf, ref n_laws in

    let rec main level todo graph asgn =
        let asgn = Array.copy asgn in
        let graph = ref graph in
        if propagate_with_memory level todo n_atoms watching graph cnf asgn
            then begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then true
                    else (let ion = ionize atom in
                        graph := ((level+1), atom, -1)::!graph;
                    	(asgn.(atom) <- ion; main (level+1) [atom] !graph asgn) ||
                        (asgn.(atom) <- -ion; main (level+1) [atom] !graph asgn))
            end else begin
                ignore(analyze_the_memory !n_clauses watching (Array.of_list !graph) cnf);
                incr n_clauses;
            	if !n_clauses = memory
                    then failwith "Clause memory overflow";
                false
            end
    in

    let asgn = Array.make (n_atoms+1) 0 in
    let graph = [] in
    main 0 [] graph asgn
;;
