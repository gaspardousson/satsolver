(* Heuristics *)

let branch n_atoms asgn =
(*
    Input:
        [int]                   Number of atoms
        [int array]             Ionization of atoms
    Output:
        [int]                   Atom to ionize
*)
    let atom = ref 1 in
    while !atom <= n_atoms && asgn.(!atom) <> 0 do
        incr atom
    done;
    !atom
;;

let ionize atom =
(*
    Input:
        [int]                   Atom to ionize
    Output:
        [int]                   Natural ion
*)
    atom
;;



(* Two watched literals *)

let search_two_watchers clause =
(*
    Input:
        [int list]              Clause
    Output:
        [int * int]             Two watchers
*)
            match clause with
                |[] -> failwith "Empty clause"
                |[ion] -> failwith "Unit clause"
                |ion1::ion2::unused when abs ion1 = abs ion2 -> failwith "Repetition"
                |ion1::ion2::unused -> abs ion1, abs ion2
;;

let init_watching n_atoms cnf =
(*
    Input:
        [int]                   Number of atoms
        [int list array]        Logical formula (cnf)
    Output:
        [int list array]        Clauses watched by atoms
*)
    let watching = Array.make (n_atoms+1) [] in
    let n_clauses = Array.length cnf in

    for clause = 0 to n_clauses-1 do
        let atom1, atom2 = search_two_watchers cnf.(clause) in
        watching.(atom1) <- clause::watching.(atom1);
        watching.(atom2) <- clause::watching.(atom2);
    done;

    watching
;;

let check_conflict cnf asgn =
(*
    Input:
        [int list array]        Logical formula (cnf)
        [int array]             Ionization of atoms
    Output:
        [bool]                  No conflict encountered?
*)
    let rec check_clause clause =
        match clause with
            |[] -> false
            |ion::clause ->
                let atom = abs ion in
                (asgn.(atom) = ion) || (asgn.(atom) = 0) || (check_clause clause)
    in

    let n_clauses = Array.length cnf in
    let no_conflict = ref true in
    for clause = 0 to n_clauses - 1 do
        no_conflict := !no_conflict && check_clause cnf.(clause)
    done;
    !no_conflict
;;

let rec propagate todo watching cnf asgn =
(*
    Input:
        [int list]              Atoms to propagate
        [int list array]        Clauses watched by atoms
        [int list array]        Logical formula (cnf)
        [int array]             Ionization of atoms
    Output:
        [bool]                  No conflict encountered?
*)
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
                match cnf.(clause) with
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
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        true, false
                                    end
                            |_ ->
                                let ion1, unused = search_new_watcher unused in
                                if ion1 = 0
                                    then false, true
                                    else begin
                                        let atom1 = abs ion1 in
                                        if asgn.(atom1) = 0
                                            then begin
                                                asgn.(atom1) <- ion1;
                                                todo := atom1::!todo;
                                            end;
                                        watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
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
                                         cnf.(clause) <- ion1::ion2::ion::unused;
                                         true, false
                                     end
                             |_ ->
                                 let ion2, unused = search_new_watcher unused in
                                 if ion2 = 0
                                     then false, true
                                     else begin
                                        let atom2 = abs ion2 in
                                        if asgn.(atom2) = 0
                                            then begin
                                                asgn.(atom2) <- ion2;
                                                todo := atom2::!todo;
                                            end;
                                        watching.(atom2) <- clause::watching.(atom2);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
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
            atom_propagated && propagate !todo watching cnf asgn
;;



(* Conflict driven clause learning *)

let extend cnf n_laws n =
    (*
        Input:
            [int list array]        Logical formula (cnf)
            [int]                   Number of original clauses
            [int]                   Number of space to add
        Output:
            [int list array]        Extended array
    *)
    let extended_cnf = Array.make (n_laws + n) [] in
    for i = 0 to n_laws-1 do
        extended_cnf.(i) <- cnf.(i)
    done;
    extended_cnf
;;

let rec propagate_with_memory level todo n_atoms watching graph cnf asgn =
(*
    Input:
        [int]                   Level of decision
        [int list]              Atoms to propagate
        [int]                   Number of atoms
        [int list array]        Clauses watched by atoms
        [int array]             Reversed adjacency list of the implication graph
        [int list array]        Logical formula (cnf)
        [int array]             Ionization of atoms
    Output:
        [bool]                  No conflict encountered?
*)
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
                match cnf.(clause) with
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
                                        graph := (level, atom2, clause)::!graph;
                                        todo := atom2::!todo;
                                        true, true
                                    end else begin
                                        let atom1 = abs ion1 in watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        true, false
                                    end
                            |_ ->
                                let ion1, unused = search_new_watcher unused in
                                if ion1 = 0
                                    then false, true
                                    else begin
                                        let atom1 = abs ion1 in
                                        if asgn.(atom1) = 0
                                            then begin
                                                asgn.(atom1) <- ion1;
                                                graph := (level, atom1, clause)::!graph;
                                                todo := atom1::!todo;
                                            end;
                                        watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
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
                                         graph := (level, atom1, clause)::!graph;
                                         todo := atom1::!todo;
                                         true, true
                                     end else begin
                                         let atom2 = abs ion2 in watching.(atom2) <- clause::watching.(atom2);
                                         cnf.(clause) <- ion1::ion2::ion::unused;
                                         true, false
                                     end
                             |_ ->
                                 let ion2, unused = search_new_watcher unused in
                                 if ion2 = 0
                                     then false, true
                                     else begin
                                        let atom2 = abs ion2 in
                                        if asgn.(atom2) = 0
                                            then begin
                                                asgn.(atom2) <- ion2;
                                                graph := (level, atom2, clause)::!graph;
                                                todo := atom2::!todo;
                                            end;
                                        watching.(atom2) <- clause::watching.(atom2);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        true, false
                                     end
                         end

                    |_ -> failwith "Atom is not watching what it supposed to"
            in

            let rec prop_atom watched =
                match watched with
                    |[] -> true, []
                    |clause::watched -> let clause_propagated, watching_it = prop_clause clause in
                                    if clause_propagated
                                    then let atom_propagated, still_watched = prop_atom watched in
                                        atom_propagated, if watching_it then clause::still_watched else still_watched
                                    else begin
                                        graph := (level, atom, clause)::!graph;
                                        false, clause::watched
                                    end
            in

            let atom_propagated, still_watched = prop_atom watching.(atom) in
            watching.(atom) <- still_watched;
            atom_propagated && propagate_with_memory level !todo n_atoms watching graph cnf asgn
;;

let analyze_the_memory position watching graph cnf =
(*
    Input:
            [int]                   Free place to remember a new clause
            [int list array]        Clauses watched by atoms
            [int * int * int array] Reversed adjacency list of the implication graph
            [int list array]        Logical formula (cnf)
    Output:
            [int]                   Number of decision levels to backjump
*)
    let learnt_clause = ref [] in
    let backjump = ref 0 in

    let n = Array.length graph in
    let c_level, c_atom, clause1 = graph.(0) in
    let bad_decision = ref false in
    for i = 1 to n-1 do
        let level, atom, clause2 = graph.(i) in
            if clause2 = -1 then bad_decision := true
                            else learnt_clause := cnf.(clause1)@cnf.(clause2)
    done;

    if !bad_decision
        then 1

        else begin
            let rec remove atom clause =
                match clause with
                    |[] -> []
                    |ion::clause when abs ion = atom -> remove atom clause
                    |ion::clause -> ion::remove atom clause
            in
            learnt_clause := remove c_atom !learnt_clause;

            let uip, c = ref 0, ref (-1) in
            for i = 1 to n-1 do
                let level, atom, clause = graph.(i) in
                if List.mem atom !learnt_clause
                    then if level = c_level
                        then begin
                            if !c <> -1 then learnt_clause := !learnt_clause@cnf.(!c);
                            learnt_clause := remove !uip !learnt_clause;
                            uip := atom; c := clause
                    end else backjump := min (c_level-level) !backjump
            done;

            let rec simplify clause =
                match clause with
                    |[] -> []
                    |ion::clause -> ion::remove (abs ion) clause
            in
            cnf.(position) <- simplify !learnt_clause;
            let atom1, atom2 = search_two_watchers cnf.(position) in
            watching.(atom1) <- position::watching.(atom1);
            watching.(atom2) <- position::watching.(atom2);

            !backjump
        end
;;