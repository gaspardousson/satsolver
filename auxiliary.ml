(* Outils pour le debug *)

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



(* Heuristiques *)

let branchement_naif n_var interpretation =
(*
    Entrée :
        [int]                       Nombre de variables
        [int array]                 Interprétation actuelle
    Sortie :
        [int]                       Variable à fixer
*)
    let var = ref 1 in
    while !var <= n_var && interpretation.(!var) <> 0 do
        incr var
    done;
    !var
;;

let rec rechauffe clause e_th =
(*
    Entrée :
        [int list]                  Clause
        [int array]                 Occurrence par variable
    Sortie :
        [unit]                      ()
*)
    match clause with
        |[] -> ()
        |lit::queue -> let var = abs lit in
                        e_th.(var) <- e_th.(var)+1;
                        rechauffe queue e_th
;;

let rec refroidit clause e_th =
(*
    Entrée :
        [int list]                  Clause
        [int array]                 Occurrence par variable
    Sortie :
        [unit]                      ()
*)
    match clause with
        |[] -> ()
        |lit::queue -> let var = abs lit in
                        e_th.(var) <- e_th.(var)-1;
                        refroidit queue e_th
;;

let init_temperature n_var n_clauses cnf =
(*
    Entrée :
        [int]                       Nombre de variables
        [int]                       Nombre de clauses
        [int list array]            CNF
    Sortie :
        [int array]                 Occurrence par variable
*)
    let e_th = Array.make (n_var+1) 0 in

    for c = 0 to n_clauses-1 do
        rechauffe cnf.(c) e_th
    done;

    e_th
;;

let branchement_th e_th n_var interpretation =
(*
    Entrée :
        [int]                       Nombre de variables
        [int array]                 Occurrence par variable
        [int array]                 Interprétation
    Sortie :
        [int]                       Variable à fixer
*)
    let var = ref 0 in
    for v = 1 to n_var do
        if interpretation.(v) = 0 && e_th.(v) > e_th.(!var)
            then var := v
    done;
    if !var = 0 then var := n_var+1;
    !var
;;

let polarisation_naive var =
(*
    Entrée :
        [int]                       Variable à fixer
    Sortie :
        [int]                       Valeur par laquelle commencer
*)
    var
;;

let init_potentiel max_clause cnf =
(*
    Entrée :
        [int]                       Nombre maximal de clauses possibles
        [int list array]            CNF
    Sortie :
        [float array]               Potentiel par clause
*)
    Array.make max_clause 0.;;
;;



(* Two watched literals *)

let cherche_deux_temoins clause =
(*
    Entrée :
        [int list]                  Clause
    Sortie :
        [int * int]                 Couple de témoins
*)
            match clause with
                |[] -> failwith "Clause vide"
                |[lit] -> abs lit, 0
                |lit1::lit2::queue when abs lit1 = abs lit2 -> failwith "Répétition"
                |lit1::lit2::queue -> abs lit1, abs lit2
;;

let init_temoignage n_var n_clauses cnf =
(*
    Entrée :
        [int]                       Nombre de variables
        [int]                       Nombre de clauses
        [int list array]            CNF
    Sortie :
        [int list array]            Témoignage (clauses par variable)
*)
    let temoignage = Array.make (n_var+1) [] in

    for c = 0 to n_clauses-1 do
        let var1, var2 = cherche_deux_temoins cnf.(c) in
        temoignage.(var1) <- c::temoignage.(var1);
        temoignage.(var2) <- c::temoignage.(var2);
    done;

    temoignage
;;

let rec cherche_un_temoin clause interpretation =
(*
    Entrée :
        [int list]                  Clause
        [int list array]            Témoignage (clauses par variable)
    Sortie :
        [int]                       Témoin
*)
    match clause with
        |[] -> 0, []
        |lit::queue ->
            let var = abs lit in
            if interpretation.(var) = -lit
                then let temoin, queue = cherche_un_temoin queue interpretation in
                    temoin, lit::queue
                else lit, queue
;;

let ne_plus_temoigner clause var temoignage =
(*
    Entrée :
        [int]                       Clause à oublier
        [int]                       Nombre de variables
        [int list array]            Témoignage  (clauses par variable)

    Sortie :
        [unit]                      ()
*)
    temoignage.(var) <- List.filter (let f c = (c <> clause) in f) temoignage.(var)
;;

let oublier clause n_var temoignage =
(*
    Entrée :
        [int]                       Clause à oublier
        [int]                       Nombre de variables
        [int list array]            Témoignage  (clauses par variable)

    Sortie :
        [unit]                      ()
*)
    for v = 1 to n_var do
        ne_plus_temoigner clause v temoignage
    done
;;

let est_sans_conflit n_clauses cnf interpretation =
(*
    Entrée :
        [int]                       Nombre de clauses
        [int list array]            CNF
        [int array]                 Interprétation
    Sortie :
        [bool]                      Aucun conflit rencontré ?
*)
    let rec peut_etre_satisfaite clause =
        match clause with
            |[] -> false
            |lit::queue ->
                let var = abs lit in
                (interpretation.(var) = lit) || (interpretation.(var) = 0) || (peut_etre_satisfaite queue)
    in

    let pas_de_conflict = ref true in
    for c = 0 to n_clauses-1 do
        pas_de_conflict := !pas_de_conflict && peut_etre_satisfaite cnf.(c)
    done;
    !pas_de_conflict
;;

let rec propagation_unitaire todo temoignage niv graphe cnf interpretation =
(*
    Entrée :
        [int list]                  Variables propagées à traiter
        [int list array]            Témoignage (clauses par variable)
        [int]                       Niveau de décision
        [(int * int * int) array]   Graphe d'implication (liste d'adjacence)
        [int list array]            CNF
        [int array]                 Interprétation
    Sortie :
        [bool]                      Aucun conflit rencontré ?
*)
    match todo with
        |[] -> true
        |var::todo ->
            let todo = ref todo in
            
            (* Vérification de la satisfaction, du caractère unitaire ou conflictuel d'une clause *)
            let propagation_clause clause =
                match cnf.(clause) with
                    |[] -> failwith "Clause vide"

                    |[lit] -> if var = 0
                                then begin
                                    let var = abs lit in
                                    ne_plus_temoigner clause var temoignage;
                                    if interpretation.(var) = 0
                                        then begin
                                            interpretation.(var) <- lit;
                                            graphe := (niv, lit, clause)::!graphe;
                                            todo := var::!todo;
                                            true
                                        end else interpretation.(var) = lit
                                end else interpretation.(var) = lit

                    |lit::lit2::queue when abs lit = var ->
                        let var2 = abs lit2 in
                        begin match interpretation.(var), interpretation.(var2) with
                            |val1, val2 when val1 = lit || val2 = lit2 ->
                                true
                            |_, 0 ->
                                let lit1, queue = cherche_un_temoin queue interpretation in
                                begin match lit1 with
                                    |0 -> interpretation.(var2) <- lit2;
                                        graphe := (niv, lit2, clause)::!graphe;
                                        todo := var2::!todo;
                                        true
                                    |_ -> let var1 = abs lit1 in temoignage.(var1) <- clause::temoignage.(var1);
                                        cnf.(clause) <- lit1::lit2::lit::queue;
                                        ne_plus_temoigner clause var temoignage;
                                        true
                                end
                            |_ ->
                                let var2 = abs lit2 in
                                let lit1, queue = cherche_un_temoin queue interpretation in
                                begin match lit1 with
                                    |0 -> graphe := (niv, -interpretation.(var2), clause)::!graphe;
                                        false
                                    |_ -> let var1 = abs lit1 in temoignage.(var1) <- clause::temoignage.(var1);
                                        cnf.(clause) <- lit1::lit2::lit::queue;
                                        ne_plus_temoigner clause var temoignage;
                                        true
                                end
                        end
                    |lit1::lit::queue when abs lit = var ->
                         let var1 = abs lit1 in
                         begin match interpretation.(var1), interpretation.(var) with
                             |val1, val2 when val1 = lit1 || val2 = lit ->
                                 true
                             |0, _ ->
                                 let lit2, queue = cherche_un_temoin queue interpretation in
                                 begin match lit2 with
                                    |0 -> interpretation.(var1) <- lit1;
                                        graphe := (niv, lit1, clause)::!graphe;
                                        todo := var1::!todo;
                                        true
                                    |_ -> let var2 = abs lit2 in temoignage.(var2) <- clause::temoignage.(var2);
                                        cnf.(clause) <- lit1::lit2::lit::queue;
                                        ne_plus_temoigner clause var temoignage;
                                        true
                                 end
                             |_ ->
                                let var1 = abs lit1 in
                                let lit2, queue = cherche_un_temoin queue interpretation in
                                begin match lit2 with
                                    |0 -> graphe := (niv, -interpretation.(var1), clause)::!graphe;
                                        false
                                    |_ -> let var2 = abs lit2 in temoignage.(var2) <- clause::temoignage.(var2);
                                        cnf.(clause) <- lit1::lit2::lit::queue;
                                        ne_plus_temoigner clause var temoignage;
                                        true
                                end
                         end
                    |_ -> failwith "Variable supposée témoin introuvable"
            in

            (* Vérification de l'absence de conflit suite à la propagation d'une variable *)
            let rec propagation_var clauses_associees =
                match clauses_associees with
                    |[] -> true
                    |clause::queue -> if propagation_clause clause
                                            then propagation_var queue
                                            else false


            in
            propagation_var temoignage.(var) && propagation_unitaire !todo temoignage niv graphe cnf interpretation
;;



(* Conflict-driven clause learning *)

let prolonger m n max_clauses e_p cnf =
(*
    Entrée :
        [int]                       Nombre de clauses prévues
        [int]                       Nombre de clauses à ajouter
        [ref int]                   Nombre maximal de clauses
        [ref (float array)]         Potentiel par clause
        [ref ((int list) array)]    CNF
    Sortie :
        [unit]                      ()
*)
    let prolongement = Array.make (m+n) [] in
    for i = 0 to m-1 do
        prolongement.(i) <- !cnf.(i)
    done;
    cnf := prolongement;

    let prolongement = Array.make (m+n) 0. in
    for i = 0 to m-1 do
        prolongement.(i) <- !e_p.(i)
    done;
    e_p := prolongement;

    max_clauses := n+m
;;

let nettoyage e_th e_p seuil n_var n_clauses max_clauses cnf temoignage =
(*
    Entrée :
        [int array]                 Occurrence par atome
        [float array]               Potentiel par clause
        [float]                     Potentiel minimal à conserver
        [int]                       Nombre de variables
        [int]                       Nombre de clauses d'origine
        [int]                       Nombre maximal de clauses
        [int list array]            CNF
        [int list array]            Témoignage (clauses par variable)
    Sortie :
        [int]                       Position pour le prochain apprentissage
*)
    let position = ref max_clauses in
    for c = n_clauses to max_clauses - 1 do
        if e_p.(c) < seuil
            then begin
                refroidit cnf.(c) e_th;
                cnf.(c) <- [];
                e_p.(c) <- 0.;
                oublier c n_var temoignage;
                position := min !position c
            end
    done;
    !position
;;

let ne_plus_apprendre litteral clause_apprise =
(*
    Entrée :
        [int]                       Littéral à ne pas retenir
        [ref (int list)]            Clause apprise

    Sortie :
        [unit]                      ()
*)
    clause_apprise := List.filter (let f l = (l <> litteral && l <> -litteral) in f) !clause_apprise
;;

let oublier_les_redondances clause_apprise =
(*
    Entrée :
        [ref (int list)]            Clause apprise

    Sortie :
        [unit]                      ()
*)
    let ajouter_si_inconnu lit clause =
        if List.mem lit clause then clause else lit::clause
    in clause_apprise := List.fold_right ajouter_si_inconnu !clause_apprise []
;;

let analyse position e_th e_p graphe cnf temoignage =
(*
    Entrée :
            [int]                       Position pour l'apprentissage
            [int array]                 Occurrence par atome
            [float array]               Potentiel par clause
            [(int * int * int) array]   Graphe d'implication (liste d'adjacence)
            [int list array]            CNF
            [int list array]            Témoignage (clauses par variable)
    Sortie :
            [backjump]                  UIP, position de la clause apprise et nombre de niveaux de décision à remonter
*)
    let clause_apprise = ref [] in
    let backjump = ref 0 in
    let clauses_utilisees = ref [] in

    let graphe = Array.of_list graphe in
    let n = Array.length graphe in
    let niv_conflit, lit1, clause1 = graphe.(0) in
    clauses_utilisees := clause1::!clauses_utilisees;

    if niv_conflit = 0

        then UNSAT

        else begin
            (* Recherche de l'autre littéral du conflit *)
            for i = 1 to n-1 do
                let niv, lit2, clause2 = graphe.(i) in
                if lit2 = -lit1
                then begin
                    clauses_utilisees := clause2::!clauses_utilisees;
                    clause_apprise := cnf.(clause1) @ cnf.(clause2)
                end
            done;

            (* Suppression du conflit de la clause apprise *)
            ne_plus_apprendre lit1 clause_apprise;

            (* Recherche de l'UIP *)
            let uip = ref 0 in
            let temoin = ref 0 in
            for i = 1 to n-1 do
                let niv1, lit1, clause1 = graphe.(i) in
                if List.mem (-lit1) !clause_apprise
                    then if niv1 = niv_conflit
                        then begin
                            let j = ref (i+1) in
                            let is_uip = ref true in
                            while !j < n && !is_uip do
                                let niv2, lit2, clause2 = graphe.(!j) in
                                is_uip := !is_uip && not (niv2 = niv1 && List.mem (-lit2) !clause_apprise);
                                incr j
                            done;

                            if !is_uip
                                then begin
                                    clauses_utilisees := clause1::!clauses_utilisees;
                                    uip := lit1
                                end else begin
                                    clause_apprise := !clause_apprise @ cnf.(clause1);
                                    ne_plus_apprendre lit1 clause_apprise;
                                end
                    end else
                        if niv1 >= !backjump
                            then (backjump := niv1; temoin := lit1)
            done;

            (* Suppression des répétitions de la clause *)
            oublier_les_redondances clause_apprise;
            ne_plus_apprendre !uip clause_apprise;
            ne_plus_apprendre !temoin clause_apprise;

            (* Incrémentation du potentiel des clauses impliquées dans l'analyse *)
            let rec eleve e_p clauses_utilisees =
                match clauses_utilisees with
                    |[] -> ()
                    |clause::queue when clause = -1 -> eleve e_p queue
                    |clause::queue -> e_p.(clause) <- e_p.(clause) +. 1.;
                                    eleve e_p queue
            in
            eleve e_p !clauses_utilisees;

            (* Choix des témoins et ajout de la clause apprise *)
            let var1 = abs !uip in
            let var2 = abs !temoin in
            if var2 = 0
                then Backjump (!uip, -1, 0)
                else begin
                    temoignage.(var1) <- position::temoignage.(var1);
                    temoignage.(var2) <- position::temoignage.(var2);
                    cnf.(position) <- (- !uip)::(- !temoin)::!clause_apprise;
                    rechauffe cnf.(position) e_th;
                    e_p.(position) <- 1.;
                    Backjump (!uip, position, !backjump)
                end;
        end
;;