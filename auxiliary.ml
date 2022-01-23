(* Debug tools *)

let print_asgn asgn =
    print_string "[|";
    print_int asgn.(0);
    for i = 1 to Array.length asgn - 1 do
        print_string ";"; print_int asgn.(i);
    done;
    print_string "|]"
;;

let print_clause clause =
    let rec aux clause =
        match clause with
            |[] -> ()
            |h::[] -> print_int h
            |h::t -> print_int h; print_string ";"; aux t
    in
    print_string "["; aux clause; print_string "]"
;;

let print_graph graph =
    let rec aux graph =
        match graph with
            |[] -> ()
            |node::[] -> let level, ion, clause = node in
                        print_string "("; print_int level; print_string ","; print_int ion; print_string ","; print_int clause; print_string ")"
            |node::graph -> let level, ion, clause = node in
                            print_string "("; print_int level; print_string ","; print_int ion; print_string ","; print_int clause; print_string ");"; aux graph
    in
    print_string "["; aux graph; print_string "]"
;;

let draw_graph path graph cnf =
    ignore (Sys.command ("touch " ^ path ^ ".gv"));
    ignore (Sys.command ("rm " ^ path ^ ".gv"));
    ignore (Sys.command ("touch " ^ path ^ ".gv"));
    let g = ref "digraph g{" in

    let rec aux1 clause n ion =
        match clause with
            |[] -> ""
            |h::t when h = ion -> aux1 t n ion
            |h::t -> (string_of_int (-h)) ^ "->" ^ (string_of_int ion) ^ "[label=c" ^ (string_of_int n) ^ "];" ^ aux1 t n ion
    in
    let rec aux2 graph =
        match graph with
            |[] -> ()
            |node::graph -> let level, ion, clause = node in
                            g := !g ^ "subgraph cluster" ^ (string_of_int level) ^ "{style=dotted;label=\"decision level=" ^ (string_of_int level) ^ "\";" ^ (string_of_int ion) ^ ";}";
                            if clause = -1
                                then g := !g ^ (string_of_int ion) ^ "[shape=rect;style=filled,fillcolor=seagreen1];"
                                else g := !g ^ (aux1 cnf.(clause) clause ion);
                            aux2 graph
    in
    aux2 graph;

    if graph <> [] then
    let rec aux3 graph anion =
        match graph with
            |[] -> ()
            |node::graph -> let level, cation, clause = node in
                            if cation = -anion
                                then g := !g
                                        ^ (string_of_int anion) ^ "->" ^ (string_of_int cation) ^ "[color=orangered;penwidth=2;dir=both;label=conflict;fontcolor=orangered];"
                                        ^ (string_of_int anion) ^ "[style=filled;fillcolor=plum1];"
                                        ^ (string_of_int cation) ^ "[style=filled;fillcolor=plum1];";
                            aux3 graph anion
    in
    let level, ion, clause = List.hd graph in
    aux3 graph ion;

    let file = open_out (path ^ ".gv") in
           output_string file (!g^"}");
    close_out file;
    ignore (Sys.command ("dot -Tpdf -o " ^ path ^ ".pdf " ^ path ^ ".gv"));
    ignore (Sys.command ("rm " ^ path ^ ".gv"));
    ignore (Sys.command ("open " ^ path ^ ".pdf"))
;;



(* Type *)
type backtrack = Sat | Backtrack;;
type backjump = SAT | Backjump of int * int * int | UNSAT;;



(* Heuristics *)

let naive_branch n_atoms asgn =
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

let rec heat e_th clause =
(*
    Input:
        [int array]             Thermal energy of atoms
        [int list]              Clause
    Output:
        [unit]                  ()
*)
    match clause with
        |[] -> ()
        |ion::clause -> let atom = abs ion in
                        e_th.(atom) <- e_th.(atom)+1;
                        heat e_th clause
;;

let rec cool e_th clause =
(*
    Input:
        [int array]             Thermal energy of atoms
        [int list]              Clause
    Output:
        [unit]                  ()
*)
    match clause with
        |[] -> ()
        |ion::clause -> let atom = abs ion in
                        e_th.(atom) <- e_th.(atom)-1;
                        cool e_th clause
;;

let init_temperature n_atoms cnf =
(*
    Input:
        [int]                   Number of atoms
        [int list array]        Logical formula (cnf)
    Output:
        [int array]             Thermal energy of atoms
*)
    let e_th = Array.make (n_atoms+1) 0 in
    let n_laws = Array.length cnf in

    for clause = 0 to n_laws-1 do
        heat e_th cnf.(clause)
    done;

    e_th
;;

let thermal_branch n_atoms e_th asgn =
(*
    Input:
        [int]                   Number of atoms
        [int array]             Thermal energy of atoms
        [int array]             Ionization of atoms
    Output:
        [int]                   Atom to ionize
*)
    let atom = ref 0 in
    for a = 1 to n_atoms do
        if asgn.(a) = 0 && e_th.(a) > e_th.(!atom)
            then atom := a
    done;
    if !atom = 0 then atom := n_atoms+1;
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

let init_potential memory cnf =
(*
    Input:
        [int]                   Number of places to learn
        [int list array]        Logical formula (cnf)
    Output:
        [float array]           Potential energy of clauses
*)
    Array.make memory 0.;;
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
                |[ion] -> abs ion, 0
                |ion1::ion2::unused when abs ion1 = abs ion2 -> failwith "Repetition"
                |ion1::ion2::unused -> abs ion1, abs ion2
;;

let init_watching n_atoms n_laws cnf =
(*
    Input:
        [int]                   Number of atoms
        [int list array]        Logical formula (cnf)
    Output:
        [int list array]        Clauses watched by atoms
*)
    let watching = Array.make (n_atoms+1) [] in

    for clause = 0 to n_laws-1 do
        let atom1, atom2 = search_two_watchers cnf.(clause) in
        watching.(atom1) <- clause::watching.(atom1);
        watching.(atom2) <- clause::watching.(atom2);
    done;

    watching
;;

let rec unwatch clause watched =
(*
    Input:
        [int]                   Clause to unwatch
        [int list]              List of previously watched clauses
    Output:
        [int list]              List of currently watched clauses
*)
    match watched with
            |[] -> []
            |c::watched when clause = c -> unwatch clause watched
            |c::watched -> c::(unwatch clause watched)
;;

let forget clause watching =
(*
    Input:
        [int]                   Clause to forget
        [int list array]        Clauses watched by atoms

    Output:
        [unit]                  ()
*)
    for atom = 1 to Array.length watching - 1 do
        watching.(atom) <- unwatch clause watching.(atom)
    done
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

    let n = Array.length cnf in
    let no_conflict = ref true in
    for clause = 0 to n-1 do
        no_conflict := !no_conflict && check_clause cnf.(clause)
    done;
    !no_conflict
;;

let rec propagate level todo watching graph cnf asgn =
(*
    Input:
        [int]                   Level of decision
        [int list]              Atoms to propagate
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

            (* Recherche d'un autre témoin dans le reste de la clause *)
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

            (* Vérification de la satisfaction, du caractère unitaire ou conflictuel d'une clause *)
            let prop_clause clause =
                match cnf.(clause) with
                    |[] -> failwith "Empty clause"

                    |[ion] -> if atom = 0
                                then begin
                                    let atom = abs ion in
                                    watching.(atom) <- unwatch clause watching.(atom);
                                    if asgn.(atom) = 0
                                        then begin
                                            asgn.(atom) <- ion;
                                            graph := (level, ion, clause)::!graph;
                                            todo := atom::!todo;
                                            true
                                        end else asgn.(atom) = ion
                                end else asgn.(atom) = ion

                    |ion::ion2::unused when abs ion = atom ->
                        let atom2 = abs ion2 in
                        begin match asgn.(atom), asgn.(atom2) with
                            |asgn1, asgn2 when asgn1 = ion || asgn2 = ion2 ->
                                true
                            |asgn1, 0 ->
                                let ion1, unused = search_new_watcher unused in
                                begin match ion1 with
                                    |0 -> asgn.(atom2) <- ion2;
                                        graph := (level, ion2, clause)::!graph;
                                        todo := atom2::!todo;
                                        true
                                    |_ -> let atom1 = abs ion1 in watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        watching.(atom) <- unwatch clause watching.(atom);
                                        true
                                end
                            |_ ->
                                let atom2 = abs ion2 in
                                let ion1, unused = search_new_watcher unused in
                                begin match ion1 with
                                    |0 -> graph := (level, -asgn.(atom2), clause)::!graph;
                                        false
                                    |_ -> let atom1 = abs ion1 in watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        watching.(atom) <- unwatch clause watching.(atom);
                                        true
                                end
                        end
                    |ion1::ion::unused when abs ion = atom ->
                         let atom1 = abs ion1 in
                         begin match asgn.(atom1), asgn.(atom) with
                             |asgn1, asgn2 when asgn1 = ion1 || asgn2 = ion ->
                                 true
                             |0, asgn2 ->
                                 let ion2, unused = search_new_watcher unused in
                                 begin match ion2 with
                                    |0 -> asgn.(atom1) <- ion1;
                                        graph := (level, ion1, clause)::!graph;
                                        todo := atom1::!todo;
                                        true
                                    |_ -> let atom2 = abs ion2 in watching.(atom2) <- clause::watching.(atom2);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        watching.(atom) <- unwatch clause watching.(atom);
                                        true
                                 end
                             |_ ->
                                let atom1 = abs ion1 in
                                let ion2, unused = search_new_watcher unused in
                                begin match ion2 with
                                    |0 -> graph := (level, -asgn.(atom1), clause)::!graph;
                                        false
                                    |_ -> let atom2 = abs ion2 in watching.(atom2) <- clause::watching.(atom2);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        watching.(atom) <- unwatch clause watching.(atom);
                                        true
                                end
                         end
                    |_ -> failwith "Watcher not found"
            in

            (* Vérification de l'absence de conflit suite à la propagation d'une variable *)
            let rec prop_atom watched =
                match watched with
                    |[] -> true
                    |clause::watched -> if prop_clause clause
                                            then prop_atom watched
                                            else false


            in
            prop_atom watching.(atom) && propagate level !todo watching graph cnf asgn
;;



(* Conflict driven clause learning *)

let extend_cnf cnf m n =
(*
    Input:
        [int list array]        Logical formula (cnf)
        [int]                   Number of current places
        [int]                   Number of places to add
    Output:
        [int list array]        Extended array
*)
    let extended_cnf = Array.make (m+n) [] in
    for i = 0 to m-1 do
        extended_cnf.(i) <- cnf.(i)
    done;
    extended_cnf
;;

let extend_e_p e_p m n =
(*
    Input:
        [float array]           Potential energy of clauses
        [int]                   Number of current places
        [int]                   Number of places to add
    Output:
        [int list array]        Extended array
*)
    let extended_e_p = Array.make (m+n) 0. in
    for i = 0 to m-1 do
        extended_e_p.(i) <- e_p.(i)
    done;
    extended_e_p
;;

let potential_clean e_th e_p ground memory n_laws watching cnf =
(*
    Input:
        [int array]             Thermal energy of atoms
        [float array]           Potential energy of clauses
        [float]                 Minimal potential energy to save
        [int]                   Number of places to learn
        [int]                   Number of original clauses
        [int list array]        Clauses watched by atoms
        [int list array]        Logical formula (cnf)
    Output:
        [int]                   Position to remember a new clause
*)
    let pos = ref memory in
    for clause = n_laws to memory - 1 do
        if e_p.(clause) < ground
            then begin
                cool e_th cnf.(clause);
                cnf.(clause) <- [];
                e_p.(clause) <- 0.;
                forget clause watching;
                pos := min !pos clause
            end
    done;
    !pos
;;

let analyze pos watching e_th e_p graph cnf =
(*
    Input:
            [int]                   Position to remember a new clause
            [int list array]        Clauses watched by atoms
            [int array]             Thermal energy of atoms
            [float array]           Potential energy of atoms
            [int * int * int array] Reversed adjacency list of the implication graph
            [int list array]        Logical formula (cnf)
    Output:
            [int]                   Number of decision levels to backjump
*)
    let learnt_clause = ref [] in
    let backjump = ref 0 in
    let used_clauses = ref [] in

    let n = Array.length graph in
    let conflict_level, anion, clause1 = graph.(0) in
    used_clauses := clause1::!used_clauses;

    if conflict_level = 0

        then UNSAT

        else begin
            (* Recherche de l'autre littéral du conflit *)
            for i = 1 to n-1 do
                let level, cation, clause2 = graph.(i) in
                if anion = -cation
                then begin
                    used_clauses := clause2::!used_clauses;
                    learnt_clause := cnf.(clause1)@cnf.(clause2)
                end
            done;

            (* Suppression du conflit de la clause apprise *)
            let rec remove ion1 clause =
                match clause with
                    |[] -> []
                    |ion2::clause when ion1 = ion2 -> remove ion1 clause
                    |ion2::clause -> ion2::remove ion1 clause
            in
            learnt_clause := remove anion !learnt_clause;
            learnt_clause := remove (-anion) !learnt_clause;

            (* Recherche de l'UIP *)
            let uip = ref 0 in
            let watcher = ref 0 in
            for i = 1 to n-1 do
                let level, cation, clause = graph.(i) in
                if List.mem (-cation) !learnt_clause
                    then if level = conflict_level
                        then begin
                            let j = ref (i+1) in
                            let is_uip = ref true in
                            while !j < n && !is_uip do
                                let l, anion, c = graph.(!j) in
                                is_uip := !is_uip && not (l = level && List.mem (-anion) !learnt_clause);
                                incr j
                            done;

                            if !is_uip
                                then begin
                                    used_clauses := clause::!used_clauses;
                                    uip := cation
                                end else begin
                                    learnt_clause := !learnt_clause@cnf.(clause);
                                    learnt_clause := remove (-cation) !learnt_clause;
                                    learnt_clause := remove (cation) !learnt_clause
                                end
                    end else
                        if level >= !backjump
                            then (backjump := level; watcher := cation)
            done;

            (* Suppression des répétitions de la clause *)
            let rec simplify clause =
                match clause with
                    |[] -> []
                    |ion::clause -> ion::simplify (remove ion clause)
            in

            learnt_clause := simplify !learnt_clause;
            learnt_clause := remove (- !uip) !learnt_clause;
            learnt_clause := remove (- !watcher) !learnt_clause;

            (* Mise à jour des occurences des variables *)
            heat e_th !learnt_clause;

            (* Incrémentation du potentiel des clauses impliquées dans l'analyse *)
            let rec rise e_p used =
                match used with
                    |[] -> ()
                    |clause::used when clause = -1 -> rise e_p used
                    |clause::used -> e_p.(clause) <- e_p.(clause) +. 1.;
                                    rise e_p used
            in
            rise e_p !used_clauses;

            (* Choix des témoins et ajout de la clause apprise *)
            let atom1 = abs !uip in
            let atom2 = abs !watcher in
            if !watcher = 0
                then Backjump (!uip, -1, 0)
                else begin
                    watching.(atom1) <- pos::watching.(atom1);
                    watching.(atom2) <- pos::watching.(atom2);
                    cnf.(pos) <- (- !uip)::(- !watcher)::!learnt_clause;
                    e_p.(pos) <- 1.;
                    Backjump (!uip, pos, !backjump)
                end;
        end
;;