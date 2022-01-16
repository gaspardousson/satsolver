open Auxiliary

(* Naive solver *)

let naive_solver problem =
    let n_atoms, n_laws, cnf = problem in

    let rec main level ion asgn =
        let asgn = Array.copy asgn in
        let atom = abs ion in asgn.(atom) <- ion;

        let atom = naive_branch n_atoms asgn in
        if atom > n_atoms
            then if check_conflict cnf asgn
                    then Sat
                    else Backtrack
            else let ion = ionize atom in
                match main (level+1) ion asgn with
                    |Sat -> Sat
                    |Backtrack ->
                match main (level+1) (-ion) asgn with
                    |Sat -> Sat
                    |Backtrack ->
                Backtrack
    in

    let asgn = Array.make (n_atoms+1) 0 in
    Sat = main 0 0 asgn
;;



(* Quine solver *)

let quine_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let e_th = init_temperature n_atoms cnf in

    let rec main level ion asgn =
        let asgn = Array.copy asgn in
        let atom = abs ion in asgn.(atom) <- ion;

        if check_conflict cnf asgn
            then begin
                let atom = thermal_branch n_atoms e_th asgn in
                if atom > n_atoms
                    then Sat
                    else let ion = ionize atom in
                        match main (level+1) ion asgn with
                            |Sat -> Sat
                            |Backtrack ->
                        match main (level+1) (-ion) asgn with
                            |Sat -> Sat
                            |Backtrack ->
                        Backtrack
            end else Backtrack
    in

    let asgn = Array.make (n_atoms+1) 0 in
    Sat = main 0 0 asgn
;;



(* DPLL solver *)

let dpll_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let watching = init_watching n_atoms n_laws cnf in
    let e_th = init_temperature n_atoms cnf in

    let rec main level ion graph asgn =
        let asgn = Array.copy asgn in
        let graph = ref graph in
        let atom = abs ion in asgn.(atom) <- ion;

        if propagate level [atom] watching graph cnf asgn
            then begin
                let atom = thermal_branch n_atoms e_th asgn in
                if atom > n_atoms
                    then Sat
                    else let ion = ionize atom in
                    	match main (level+1) ion ((level+1,ion,-1)::!graph) asgn with
                    	    |Sat -> Sat
                    	    |Backtrack ->
                    	match main (level+1) (-ion) ((level+1,-ion,-1)::!graph) asgn with
                    	    |Sat -> Sat
                    	    |Backtrack ->
                        Backtrack
            end else Backtrack
    in

    let asgn = Array.make (n_atoms+1) 0 in
    Sat = main 0 0 [] asgn
;;



(* CDCL solver *)

let cdcl_solver problem =
    let n_atoms, n_laws, cnf = problem in
    let watching = init_watching n_atoms n_laws cnf in
    let e_th = init_temperature n_atoms cnf in

    let cnf = ref (extend cnf n_laws (n_laws/3)) in
    let memory, pos = ref (Array.length !cnf), ref n_laws in

    let rec main level ion graph asgn =
        let asgn = Array.copy asgn in
        let graph = ref graph in
        let atom = abs ion in asgn.(atom) <- ion;

        if propagate level [atom] watching graph !cnf asgn
            then begin
                let atom = thermal_branch n_atoms e_th asgn in
                if atom > n_atoms
                    then SAT
                    else let ion = ionize atom in
                    	match main (level+1) ion ((level+1,ion,-1)::!graph) asgn with
                    	    |SAT -> SAT
                    	    |Backjump (uip, c, l) ->
                    	        if l = level then begin
                                    main level (-uip) ((level,-uip,c)::!graph) asgn
                                end else Backjump (uip, c, l)
                            |UNSAT -> UNSAT
            end else begin
                let backjump = analyze !pos watching e_th (Array.of_list !graph) !cnf in
                incr pos;
            	if !pos >= !memory
                    then begin
                        cnf := extend !cnf !memory (!memory/10);
                        memory := (Array.length !cnf)
                    end;
                backjump
            end
    in

    let asgn = Array.make (n_atoms+1) 0 in
    SAT = (main 0 0 [] asgn)
;;
