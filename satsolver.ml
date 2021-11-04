(* Naive solver *)
open Int

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
        for i = 0 to Array.length cnf - 1 do
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
    let n = Array.length formula in
    let satisfied = Array.make n 0 in

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



(* DPLL solver *)

let dpll_solver problem =
    let n_atoms, form = problem in
    let n_clauses = Array.length form in
    let watching = Array.make (n_atoms+1) [] in

    (* prop *)
    let rec propagate todo asgn =
        match todo with
            |[] -> true
            |atom::todo ->
                let todo = ref todo in
    
                let rec search_new_watcher unused =
                    match unused with
                        |[] -> 0, []
                        |new_ion::unused ->
                            let new_atom = abs new_ion in
                            if asgn.(new_atom) = -new_ion
                                then let watcher, unused = search_new_watcher unused in
                                    watcher, new_ion::unused
                                else new_ion, unused
                in
    
                let prop_clause clause =
                    match form.(clause) with
                        |[] -> failwith "Empty clause"
                        |[ion] -> failwith "Unit clause"

                        |ion::ion2::unused when abs ion = atom ->
                            let atom2 = abs ion2 in
                            begin match asgn.(atom), asgn.(atom2) with
                                |asgn1, asgn2 when asgn1 = ion || asgn2 = ion2 ->
                                    true, true
                                |asgn1, 0 ->
                                    let ion1, unused = search_new_watcher unused in
                                    if ion1 = 0
                                        then begin
                                            asgn.(atom2) <- ion2;
                                            todo := atom2::!todo;
                                            true, true
                                        end else begin
                                            let atom1 = abs ion1 in watching.(atom1) <- clause::watching.(atom1);
                                            form.(clause) <- ion1::ion2::ion::unused;
                                            true, false
                                        end
                                |_ ->
                                    let ion1, unused = search_new_watcher unused in
                                    if ion1 = 0
                                        then false, true
                                        else begin
                                            let atom1 = abs ion1 in asgn.(atom1) <- ion1;
                                            watching.(atom1) <- clause::watching.(atom1);
                                            form.(clause) <- ion1::ion2::ion::unused;
                                            todo := atom1::!todo;
                                            true, false
                                        end
                            end

                        |ion1::ion::unused when abs ion = atom ->
                             let atom1 = abs ion1 in
                             begin match asgn.(atom1), asgn.(atom) with
                                 |asgn1, asgn2 when asgn1 = ion1 || asgn2 = ion ->
                                     true, true
                                 |0, asgn2 ->
                                     let ion2, unused = search_new_watcher unused in
                                     if ion2 = 0
                                         then begin
                                             asgn.(atom1) <- ion1;
                                             todo := atom1::!todo;
                                             true, true
                                         end else begin
                                             let atom2 = abs ion2 in watching.(atom2) <- clause::watching.(atom2);
                                             form.(clause) <- ion1::ion2::ion::unused;
                                             true, false
                                         end
                                 |_ ->
                                     let ion2, unused = search_new_watcher unused in
                                     if ion2 = 0
                                         then false, true
                                         else begin
                                            let atom2 = abs ion2 in asgn.(atom2) <- ion2;
                                            watching.(atom2) <- clause::watching.(atom2);
                                            form.(clause) <- ion1::ion2::ion::unused;
                                            todo := atom2::!todo;
                                            true, false
                                         end
                             end

                        |_ -> failwith "Watcher not found"
                in
    
                let rec prop_atom watched =
                    match watched with
                        |[] -> true, []
                        |clause::watched -> let clause_propagated, watching_it = prop_clause clause in
                                        if clause_propagated
                                        then let atom_propagated, still_watched = prop_atom watched in
                                            atom_propagated, if watching_it then clause::still_watched else still_watched
                                        else false, clause::watched
                in
    
                let atom_propagated, still_watched = prop_atom watching.(atom) in
                watching.(atom) <- still_watched;
                atom_propagated && propagate !todo asgn
    in

    (* main *)
    let rec main todo asgn =
        let asgn = Array.copy asgn in
        if propagate todo asgn
            then begin
                let atom = ref 1 in
                while !atom <= n_atoms && asgn.(!atom) <> 0 do
                    incr atom
                done;
                (!atom > n_atoms) ||
                (asgn.(!atom) <- !atom; main [!atom] asgn) ||
                (asgn.(!atom) <- - !atom; main [!atom] asgn)
            end
            else false
    in

    (* init *)
    let asgn = Array.make (n_atoms+1) 0 in

    let search_two_watchers clause =
            match clause with
                |[] -> failwith "Empty clause"
                |[ion] -> failwith "Unit clause"
                |ion1::ion2::unused when abs ion1 = abs ion2 -> failwith "Repetition"
                |ion1::ion2::unused -> ion1, ion2
    in

    for clause = 0 to n_clauses-1 do
        let ion1, ion2 = search_two_watchers form.(clause) in
        let atom1, atom2 = abs ion1, abs ion2 in
        watching.(atom1) <- clause::watching.(atom1);
        watching.(atom2) <- clause::watching.(atom2);
    done;

    main [] asgn
;;



(* TODO: CDCL solver *)

let cdcl_solver problem =
    ()
;;
