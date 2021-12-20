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

let init_watching n_atoms cnf =
(*
    Input:
        [int]                   Number of atoms
        [int list array]        Logical formula (cnf)
    Output:
        [int list array]        Clauses watched by atoms
*)
    let asgn = Array.make (n_atoms+1) 0 in
    let watching = Array.make (n_atoms+1) [] in
    let n_clauses = Array.length cnf in

    let search_two_watchers clause =
            match clause with
                |[] -> failwith "Empty clause"
                |[ion] -> failwith "Unit clause"
                |ion1::ion2::unused when abs ion1 = abs ion2 -> failwith "Repetition"
                |ion1::ion2::unused -> ion1, ion2
    in

    for clause = 0 to n_clauses-1 do
        let ion1, ion2 = search_two_watchers cnf.(clause) in
        let atom1, atom2 = abs ion1, abs ion2 in
        watching.(atom1) <- clause::watching.(atom1);
        watching.(atom2) <- clause::watching.(atom2);
    done;

    asgn, watching
;;

let check_conflict cnf asgn =
(*
    Input:
        [int list array]        Logical formula (cnf)
        [int array]             Ionization of atoms
    Output:
        [bool]                  Conflict encountered?
*)
    let rec check_clause clause =
        match clause with
            |[] -> true
            |ion::clause ->
                let atom = abs ion in
                (asgn.(atom) = -ion) && (check_clause clause)
    in

    let n_clauses = Array.length cnf in
    let conflict = ref false in
    for clause = 0 to n_clauses - 1 do
        if not !conflict
        then conflict := check_clause cnf.(clause)
    done;
    !conflict
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
                                        let atom1 = abs ion1 in asgn.(atom1) <- ion1;
                                        watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
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
                                         cnf.(clause) <- ion1::ion2::ion::unused;
                                         true, false
                                     end
                             |_ ->
                                 let ion2, unused = search_new_watcher unused in
                                 if ion2 = 0
                                     then false, true
                                     else begin
                                        let atom2 = abs ion2 in asgn.(atom2) <- ion2;
                                        watching.(atom2) <- clause::watching.(atom2);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
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
            atom_propagated && propagate !todo watching cnf asgn
;;