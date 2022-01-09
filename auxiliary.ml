(* Debug tools *)

let print_asgn asgn =
    print_string "[";
    print_int asgn.(0);
    for i = 1 to Array.length asgn - 1 do
        print_string ";"; print_int asgn.(i);
    done;
    print_string "]"
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

let occ_branch n_atoms occ asgn =
(*
    Input:
        [int]                   Number of atoms
        [int array]             Ionization of atoms
        [int array]             Occurrences of atoms
    Output:
        [int]                   Atom to ionize
*)
    let atom = ref 0 in
    for a = 1 to n_atoms do
        if asgn.(a) = 0 && occ.(a) > occ.(!atom)
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

let init_occ n_atoms cnf =
(*
    Input:
        [int]                   Number of atoms
        [int list array]        Logical formula (cnf)
    Output:
        [int * int]             Two watchers
*)
    let occ = Array.make (n_atoms+1) 0 in
    let n_laws = Array.length cnf in

    let rec count occ clause =
        match clause with
            |[] -> ()
            |ion::clause -> let atom = abs ion in
                            occ.(atom) <- occ.(atom)+1;
                            count occ clause
    in

    for clause = 0 to n_laws-1 do
        count occ cnf.(clause)
    done;

    occ
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

                    |[ion] -> if atom = 0
                                then let atom = abs ion in
                                     if asgn.(atom) = 0
                                        then begin
                                            asgn.(atom) <- ion;
                                            graph := (level, ion, clause)::!graph;
                                            todo := atom::!todo;
                                            true, false
                                        end else asgn.(atom) = ion, false
                                else asgn.(atom) = ion, true

                    |ion::ion2::unused when abs ion = atom ->
                        let atom2 = abs ion2 in
                        begin match asgn.(atom), asgn.(atom2) with
                            |asgn1, asgn2 when asgn1 = ion || asgn2 = ion2 ->
                                true, true
                            |asgn1, 0 ->
                                let ion1, unused = search_new_watcher unused in
                                begin match ion1 with
                                    |0 -> asgn.(atom2) <- ion2;
                                        graph := (level, ion2, clause)::!graph;
                                        todo := atom2::!todo;
                                        true, true
                                    |_ -> let atom1 = abs ion1 in watching.(atom1) <- clause::watching.(atom1);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        true, false
                                end
                            |_ ->
                                let atom2 = abs ion2 in
                                let ion1, unused = search_new_watcher unused in
                                begin match ion1 with
                                    |0 -> graph := (level, -asgn.(atom2), clause)::!graph;
                                        false, true
                                    |_ -> let atom1 = abs ion1 in watching.(atom1) <- clause::watching.(atom1);
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
                                 begin match ion2 with
                                    |0 -> asgn.(atom1) <- ion1;
                                        graph := (level, ion1, clause)::!graph;
                                        todo := atom1::!todo;
                                        true, true
                                    |_ -> let atom2 = abs ion2 in watching.(atom2) <- clause::watching.(atom2);
                                        cnf.(clause) <- ion1::ion2::ion::unused;
                                        true, false
                                 end
                             |_ ->
                                let atom1 = abs ion1 in
                                let ion2, unused = search_new_watcher unused in
                                begin match ion2 with
                                    |0 -> graph := (level, -asgn.(atom1), clause)::!graph;
                                        false, true
                                    |_ -> let atom2 = abs ion2 in watching.(atom2) <- clause::watching.(atom2);
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
            atom_propagated && propagate level !todo watching graph cnf asgn
;;



(* Conflict driven clause learning *)

let extend cnf m n =
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

(* TODO: in O(V+E) *)
let analyze n_clauses watching occ graph cnf =
(*
    Input:
            [int]                   Free place to remember a new clause
            [int list array]        Clauses watched by atoms
            [int array]             Occurrences of atoms
            [int * int * int array] Reversed adjacency list of the implication graph
            [int list array]        Logical formula (cnf)
    Output:
            [int]                   Number of decision levels to backjump
*)
    let learnt_clause = ref [] in
    let backjump = ref 0 in

    let n = Array.length graph in
    let c_level, anion, clause1 = graph.(0) in

    if c_level = 0

        then UNSAT

        else begin
            for i = 1 to n-1 do
                let level, cation, clause2 = graph.(i) in
                if anion = -cation
                then learnt_clause := cnf.(clause1)@cnf.(clause2)
            done;

            let rec remove anion clause =
                match clause with
                    |[] -> []
                    |cation::clause when cation = anion -> remove cation clause
                    |cation::clause -> cation::remove anion clause
            in
            learnt_clause := remove anion !learnt_clause;
            learnt_clause := remove (-anion) !learnt_clause;

            let uip = ref 0 in
            let watcher = ref 0 in
            for i = 1 to n-1 do
                let level, cation, clause = graph.(i) in
                if List.mem (-cation) !learnt_clause
                    then if level = c_level
                        then begin
                            let j = ref (i+1) in
                            let is_uip = ref true in
                            while !j < n && !is_uip do
                                let l, anion, c = graph.(!j) in
                                is_uip := !is_uip && not (l = level && List.mem (-anion) !learnt_clause);
                                incr j
                            done;

                            if !is_uip
                                then uip := cation
                                else begin
                                    learnt_clause := !learnt_clause@cnf.(clause);
                                    learnt_clause := remove (-cation) !learnt_clause;
                                    learnt_clause := remove (cation) !learnt_clause
                                end
                    end else
                        if level >= !backjump
                            then (backjump := level; watcher := cation)
            done;

            let rec simplify clause =
                match clause with
                    |[] -> []
                    |ion::clause -> ion::simplify (remove ion clause)
            in

            learnt_clause := simplify !learnt_clause;
            learnt_clause := remove (- !uip) !learnt_clause;
            learnt_clause := remove (- !watcher) !learnt_clause;

            let rec count occ clause =
                match clause with
                    |[] -> ()
                    |ion::clause -> let atom = abs ion in
                                    occ.(atom) <- occ.(atom)+1;
                                    count occ clause
            in
            count occ !learnt_clause;

            let atom1 = abs !uip in
            let atom2 = abs !watcher in

            if !watcher = 0
                then Backjump (!uip, -1, 0)
                else begin
                    watching.(atom1) <- !n_clauses::watching.(atom1);
                    watching.(atom2) <- !n_clauses::watching.(atom2);
                    cnf.(!n_clauses) <- (- !uip)::(- !watcher)::!learnt_clause;
                    incr n_clauses;
                    Backjump (!uip, !n_clauses-1, !backjump)
                end;
        end
;;