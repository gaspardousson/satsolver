(* Outils pour le debug *)

let print_interpretation interpretation =
    print_string "[|";
    print_int interpretation.(0);
    for i = 1 to Array.length interpretation - 1 do
        print_string ";"; print_int interpretation.(i);
    done;
    print_string "|]"
;;


let print_clause clause =
    let rec aux clause =
        match clause with
            |[] -> ()
            |lit::[] -> print_int lit
            |lit::queue -> print_int lit; print_string ";"; aux queue
    in
    print_string "["; aux clause; print_string "]"
;;


let print_graphe graphe =
    let rec aux graphe =
        match graphe with
            |[] -> ()
            |noeud::[] -> let niv, lit, clause = noeud in
                        print_string "("; print_int niv; print_string ","; print_int lit; print_string ","; print_int clause; print_string ")"
            |noeud::g -> let niv, lit, clause = noeud in
                        print_string "("; print_int niv; print_string ","; print_int lit; print_string ","; print_int clause; print_string ");"; aux g
    in
    print_string "["; aux graphe; print_string "]"
;;


let tracer_graphe chemin graphe n_clauses cnf =
    ignore (Sys.command ("touch " ^ chemin ^ ".gv"));
    ignore (Sys.command ("rm " ^ chemin ^ ".gv"));
    ignore (Sys.command ("touch " ^ chemin ^ ".gv"));
    let g = ref "digraph g{" in

    let rec aux1 clause n lit1 =
        match clause with
            |[] -> ""
            |lit2::queue when lit1 = lit2 -> aux1 queue n lit1
            |lit2::queue when n <= n_clauses -> (string_of_int (-lit2)) ^ "->" ^ (string_of_int lit1) ^ "[label=c" ^ (string_of_int n) ^ "];" ^ aux1 queue n lit1
            |lit2::queue -> (string_of_int (-lit2)) ^ "->" ^ (string_of_int lit1) ^ "[label=a" ^ (string_of_int n) ^ "];" ^ aux1 queue n lit1
        in
    let rec aux2 graphe =
        match graphe with
            |[] -> ()
            |noeud::graphe -> let niv, lit, clause = noeud in
                            g := !g ^ "subgraph cluster" ^ (string_of_int niv) ^ "{style=dotted;label=\"niveau de décision=" ^ (string_of_int niv) ^ "\";" ^ (string_of_int lit) ^ ";}";
                            if clause = -1
                                then g := !g ^ (string_of_int lit) ^ "[shape=rect;style=filled,fillcolor=seagreen1];"
                                else g := !g ^ (aux1 cnf.(clause) clause lit);
                            aux2 graphe
    in
    aux2 graphe;

    if graphe <> [] then
    let rec aux3 graphe lit1 =
        match graphe with
            |[] -> ()
            |noeud::graphe -> let niv, lit2, clause = noeud in
                            if lit1 = -lit2
                                then g := !g
                                        ^ (string_of_int lit1) ^ "->" ^ (string_of_int lit2) ^ "[color=orangered;penwidth=2;dir=both;label=conflit;fontcolor=orangered];"
                                        ^ (string_of_int lit1) ^ "[style=filled;fillcolor=plum1];"
                                        ^ (string_of_int lit2) ^ "[style=filled;fillcolor=plum1];";
                            aux3 graphe lit1
    in
    let niv, lit, clause = List.hd graphe in
    aux3 graphe lit;

    let dessin = open_out (chemin ^ ".gv") in
           output_string dessin (!g^"}");
    close_out dessin;
    ignore (Sys.command ("dot -Tpdf -o " ^ chemin ^ ".pdf " ^ chemin ^ ".gv"));
    ignore (Sys.command ("rm " ^ chemin ^ ".gv"));
    ignore (Sys.command ("open " ^ chemin ^ ".pdf"))
;;



(* Calculs statistiques *)

let moyenne t =
(* Calcule la valeur moyenne d'un tableau. *)
    let n = Array.length t in
    let m = ref 0. in
    for i = 0 to n - 1 do
        m := !m +. t.(i)
    done;
    !m /. float_of_int n
;;

let ecart_type t =
(* Calcule une estimation de l'écart-type d'un tableau. *)
    let n = Array.length t in
    let s = ref 0. in
    let m = moyenne t in
    for i = 0 to n - 1 do
        s := !s +. (m -. t.(i)) ** 2.
    done;
    sqrt (!s /. float_of_int (n-1))
;;

let mediane t =
(* Calcule la valeur médiane d'un tableau. *)
    let comparaison x y =
        if x < y then 1 else 0
    in
    let u = Array.copy t in
    Array.fast_sort comparaison u;
    u.(Array.length u / 2)
;;



(* Types *)
type backtrack = Sat | Backtrack;;
type backjump = SAT | Backjump of int * int * int | UNSAT;;



(* Heuristiques *)

let branchement_naif n_var interpretation =
(* Renvoie la première variable sans valeur dans l'ordre croissant d'indice. *)
    let var = ref 1 in
    while !var <= n_var && interpretation.(!var) <> 0 do
        incr var
    done;
    !var
;;


let rec incrementer occurrence clause =
(* Incrémente l'occurrence des variables apparaissant dans la clause. *)
    match clause with
        |[] -> ()
        |lit::queue -> let var = abs lit in
                        occurrence.(var) <- occurrence.(var)+1;
                        incrementer occurrence queue
;;


let rec decrementer occurrence clause =
(* Décrémente l'occurrence des variables apparaissant dans la clause. *)
    match clause with
        |[] -> ()
        |lit::queue -> let var = abs lit in
                        occurrence.(var) <- occurrence.(var)-1;
                        decrementer occurrence queue
;;


let init_occurrence n_var n_clauses cnf =
(* Renvoie le tableau d'occurrence de chaque variable dans les clauses de la formule. *)
    let occurrence = Array.make (n_var+1) 0 in

    for c = 0 to n_clauses-1 do
        incrementer occurrence cnf.(c)
    done;

    occurrence
;;


let branchement_occ n_var interpretation occurrence =
(* Renvoie la première variable sans valeur par ordre décroissant d'occurrence. *)
    let var = ref 0 in
    for v = 1 to n_var do
        if interpretation.(v) = 0 && occurrence.(v) > occurrence.(!var)
            then var := v
    done;
    if !var = 0 then var := n_var+1;
    !var
;;


let init_polarisation n_var =
(* Renvoie le tableau des littéraux positifs pour la polarisation initiale. *)
    let derniere_prop = Array.make (n_var+1) 0 in

    for v = 1 to n_var do
        derniere_prop.(v) <- v
    done;

    derniere_prop
;;


let init_activite max_clause cnf =
(* Renvoie l'activité initiale des clauses de la formule. *)
    Array.make max_clause 0.;;
;;



(* Two watched literals *)

let cherche_deux_temoins clause =
(* Renvoie deux témoins distincts pour la clause (ignore les valuations). *)
            match clause with
                |[] -> failwith "Clause vide"
                |[lit] -> abs lit, 0
                |lit1::lit2::queue when abs lit1 = abs lit2 -> failwith "Répétition"
                |lit1::lit2::queue -> abs lit1, abs lit2
;;


let init_temoignage n_var n_clauses cnf =
(* Renvoie le tableau des clauses témoignées par chaque variable. *)
    let temoignage = Array.make (n_var+1) [] in

    for c = 0 to n_clauses-1 do
        let var1, var2 = cherche_deux_temoins cnf.(c) in
        temoignage.(var1) <- c::temoignage.(var1);
        temoignage.(var2) <- c::temoignage.(var2);
    done;

    temoignage
;;


let rec cherche_un_temoin clause interpretation =
(* Renvoie un témoin pour la clause. *)
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
(* Supprime la clause de la liste des clauses témoignée par la variable. *)
    temoignage.(var) <- List.filter (let f c = (c <> clause) in f) temoignage.(var)
;;


let oublier clause n_var temoignage =
(* Supprime la clause de toutes les listes de clauses témoignées. *)
    for v = 1 to n_var do
        ne_plus_temoigner clause v temoignage
    done
;;


let est_sans_conflit n_clauses cnf interpretation =
(* Renvoie un booléen indiquant si l'interprétation ne cause aucun conflit. *)
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

let rec propagation_unitaire todo interpretation cnf ?n:(niv = -1) ?g:(graphe=ref []) ?p:(polarisation=[||]) temoignage =
(* Réalise la propagation unitaire et renvoie un booléen indiquant si aucun conflit n'a été rencontré. *)
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
                                            if polarisation <> [||] then polarisation.(var) <- lit;
                                            if niv <> -1 then graphe := (niv, lit, clause)::!graphe;
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
                                        if polarisation <> [||] then polarisation.(var2) <- lit2;
                                        if niv <> -1 then graphe := (niv, lit2, clause)::!graphe;
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
                                    |0 -> if niv <> -1 then graphe := (niv, -interpretation.(var2), clause)::!graphe;
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
                                        if polarisation <> [||] then polarisation.(var1) <- lit1;
                                        if niv <> -1 then graphe := (niv, lit1, clause)::!graphe;
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
                                    |0 -> if niv <> -1 then graphe := (niv, -interpretation.(var1), clause)::!graphe;
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
            propagation_var temoignage.(var) && propagation_unitaire !todo interpretation cnf ~n:niv ~g:graphe ~p:polarisation temoignage
;;



(* Conflict-driven clause learning *)

let prolonger m n max_clauses ?a:(activite=ref [||]) cnf =
(* Ajoute n emplacements au tableau de clauses et au tableau d'activités des clauses. *)
    let prolongement = Array.make (m+n) [] in
    for i = 0 to m-1 do
        prolongement.(i) <- !cnf.(i)
    done;
    cnf := prolongement;

    if !activite <> [||] then begin
        let prolongement = Array.make (m+n) 0. in
        for i = 0 to m-1 do
            prolongement.(i) <- !activite.(i)
        done;
        activite := prolongement
    end;

    max_clauses := m+n
;;


let nettoyage seuil activite n_var n_clauses max_clauses graphe cnf ?o:(occurrence=[||]) temoignage =
(* Supprime toutes occurrences des clauses dont l'activité est inférieure au seuil (clauses du graphe exclues) et renvoie le plus petit indice indiquant un emplacement vide. *)
    let rec conserver graphe =
        match graphe with
            |[] -> []
            |(_, _, c)::g -> c::(conserver g)
    in
    let inoubliables = conserver graphe in
    let position = ref max_clauses in
    for c = n_clauses to max_clauses - 1 do
        if activite.(c) < seuil && not (List.mem c inoubliables)
            then begin
                if occurrence <> [||] then decrementer occurrence cnf.(c);
                cnf.(c) <- [];
                activite.(c) <- 0.;
                oublier c n_var temoignage;
                position := min !position c
            end
    done;
    !position
;;


let ne_plus_apprendre litteral clause_apprise =
(* Supprime toutes les occurrences du littéral ou de sa négation dans la clause apprise. *)
    clause_apprise := List.filter (let f l = (l <> litteral && l <> -litteral) in f) !clause_apprise
;;


let supprimer_les_redondances clause_apprise =
(* Supprime toutes les redondances dans la clause apprise. *)
    let ajouter_si_inconnu lit clause =
        if List.mem lit clause then clause else lit::clause
    in clause_apprise := List.fold_right ajouter_si_inconnu !clause_apprise []
;;



let analyse position graphe ?o:(occurrence=[||]) ?a:(activite=[||]) cnf temoignage =
(* Analyse le conflit, apprend la clause correspondante et renvoie l'UIP, la position de la clause apprise et le backjump à réaliser. *)
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
            supprimer_les_redondances clause_apprise;
            ne_plus_apprendre !uip clause_apprise;
            ne_plus_apprendre !temoin clause_apprise;

            (* Incrémentation du potentiel des clauses impliquées dans l'analyse *)
            if activite <> [||] then begin
                let rec eleve activite clauses_utilisees =
                    match clauses_utilisees with
                        |[] -> ()
                        |clause::queue when clause = -1 -> eleve activite queue
                        |clause::queue -> activite.(clause) <- activite.(clause) +. 1.;
                                    eleve activite queue
                in
                eleve activite !clauses_utilisees
            end;

            (* Choix des témoins et ajout de la clause apprise *)
            let var1 = abs !uip in
            let var2 = abs !temoin in
            if var2 = 0
                then Backjump (!uip, -1, 0)
                else begin
                    temoignage.(var1) <- position::temoignage.(var1);
                    temoignage.(var2) <- position::temoignage.(var2);
                    cnf.(position) <- (- !uip)::(- !temoin)::!clause_apprise;
                    if occurrence <> [||] then incrementer occurrence cnf.(position);
                    if activite <> [||] then activite.(position) <- 1.;
                    Backjump (!uip, position, !backjump)
                end;
        end
;;
