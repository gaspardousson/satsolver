open Auxiliary

(* Naive solver *)

let naive_solver problem =
    let n_atoms, n_laws, cnf = problem in

    let rec main level todo asgn =
        let asgn = Array.copy asgn in
        let atom = branch n_atoms asgn in
        if atom > n_atoms
            then if check_conflict cnf asgn
                    then Sat
                    else Backtrack
            else let ion = ionize atom in
                match (asgn.(atom) <- ion; main (level+1) [atom] asgn) with
                    |Sat -> Sat
                    |Backtrack ->
                match (asgn.(atom) <- -ion; main (level+1) [atom] asgn) with
                    |Sat -> Sat
                    |Backtrack ->
                Backtrack
    in

    let asgn = Array.make (n_atoms+1) 0 in
    Sat = main 0 [] asgn
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
                    then Sat
                    else let ion = ionize atom in
                        match (asgn.(atom) <- ion; main (level+1) [atom] asgn) with
                            |Sat -> Sat
                            |Backtrack ->
                        match (asgn.(atom) <- -ion; main (level+1) [atom] asgn) with
                            |Sat -> Sat
                            |Backtrack ->
                        Backtrack
            end else Backtrack
    in

    let asgn = Array.make (n_atoms+1) 0 in
    Sat = main 0 [] asgn
;;



(* DPLL solver *)

let dpll_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let watching = init_watching n_atoms cnf in

    let rec main level todo asgn =
        let asgn = Array.copy asgn in
        if propagate level todo watching (ref []) cnf asgn
            then begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then Sat
                    else let ion = ionize atom in
                    	match (asgn.(atom) <- ion; main (level+1) [atom] asgn) with
                    	    |Sat -> Sat
                    	    |Backtrack ->
                    	match (asgn.(atom) <- -ion; main (level+1) [atom] asgn) with
                    	    |Sat -> Sat
                    	    |Backtrack ->
                        Backtrack
            end else Backtrack
    in

    let asgn = Array.make (n_atoms+1) 0 in
    Sat = main 0 [] asgn
;;



(* CDCL solver *)

let cdcl_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let watching = init_watching n_atoms cnf in

    let cnf = extend cnf n_laws 1000000 in
    let memory, n_clauses = Array.length cnf, ref n_laws in

    let rec main level todo graph asgn =
        let asgn = Array.copy asgn in
        let graph = ref graph in
        if propagate level todo watching graph cnf asgn
            then begin
                let atom = branch n_atoms asgn in
                if atom > n_atoms
                    then SAT
                    else let ion = ionize atom in
                    	match (asgn.(atom) <- ion; main (level+1) [atom] ((level+1,ion,-1)::!graph) asgn) with
                    	    |SAT -> SAT
                    	    |Backjump (uip, c, l) ->
                    	        asgn.(atom) <- 0;
                    	        if l = level then begin
                    	            let atom = abs uip in
                                    asgn.(atom) <- -uip;
                                    main level [atom] ((level,-uip,c)::!graph) asgn
                                end else Backjump (uip, c, l)
                            |UNSAT -> UNSAT
            end else begin
                incr n_clauses;
            	if !n_clauses > memory
                    then failwith "Clause memory overflow";
                analyze (!n_clauses-1) watching (Array.of_list !graph) cnf
            end
    in

    let asgn = Array.make (n_atoms+1) 0 in
    let graph = [] in
    SAT = (main 0 [] graph asgn)
;;
