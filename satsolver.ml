open Auxiliary

(* Solveur naïf *)

let solveur_naif probleme =
    let n_var, n_clauses, cnf = probleme in
    let interpretation = Array.make (n_var+1) 0 in

    let rec main niv lit interpretation =
        let interpretation = Array.copy interpretation in
        let var = abs lit in interpretation.(var) <- lit;

        let var = branchement_naif n_var interpretation in
        if var > n_var
            then if est_sans_conflit n_clauses cnf interpretation
                    then Sat
                    else Backtrack
            else let lit = polarisation_naive var in
                match main (niv+1) lit interpretation with
                    |Sat -> Sat
                    |Backtrack ->
                match main (niv+1) (-lit) interpretation with
                    |Sat -> Sat
                    |Backtrack ->
                Backtrack
    in

    Sat = main 0 0 interpretation
;;



(* Méthode de Quine *)

let solveur_quine probleme =
    let n_var, n_clauses, cnf = probleme in
    let interpretation = Array.make (n_var+1) 0 in
    let e_th = init_temperature n_var n_clauses cnf in

    let rec main niv lit interpretation =
        let interpretation = Array.copy interpretation in
        let var = abs lit in interpretation.(var) <- lit;

        if est_sans_conflit n_clauses cnf interpretation
            then begin
                let var = branchement_th e_th n_var interpretation in
                if var > n_var
                    then Sat
                    else let lit = polarisation_naive var in
                        match main (niv+1) lit interpretation with
                            |Sat -> Sat
                            |Backtrack ->
                        match main (niv+1) (-lit) interpretation with
                            |Sat -> Sat
                            |Backtrack ->
                        Backtrack
            end else Backtrack
    in

    Sat = main 0 0 interpretation
;;



(* Algorithme DPLL *)

let solveur_dpll probleme =
    let n_var, n_clauses, cnf = probleme in
    let interpretation = Array.make (n_var+1) 0 in
    let temoignage = init_temoignage n_var n_clauses cnf in
    let e_th = init_temperature n_var n_clauses cnf in

    let rec main niv lit graphe interpretation =
        let interpretation = Array.copy interpretation in
        let graphe = ref graphe in
        let var = abs lit in interpretation.(var) <- lit;

        if propagation_unitaire [var] temoignage niv graphe cnf interpretation
            then begin
                let var = branchement_th e_th n_var interpretation in
                if var > n_var
                    then Sat
                    else let lit = polarisation_naive var in
                    	match main (niv+1) lit ((niv+1,lit,-1)::!graphe) interpretation with
                    	    |Sat -> Sat
                    	    |Backtrack ->
                    	match main (niv+1) (-lit) ((niv+1,-lit,-1)::!graphe) interpretation with
                    	    |Sat -> Sat
                    	    |Backtrack ->
                        Backtrack
            end else Backtrack
    in

    Sat = main 0 0 [] interpretation
;;



(* Conflict-driven clause learning *)

let solveur_cdcl probleme =
    let n_var, n_clauses, cnf = probleme in
    let interpretation = Array.make (n_var+1) 0 in
    let temoignage = init_temoignage n_var n_clauses cnf in
    let e_th = init_temperature n_var n_clauses cnf in

    let cnf = ref cnf in
    let max_clauses, position = ref n_clauses, ref n_clauses in
    let e_p = ref (init_potentiel !max_clauses cnf) in
    prolonger n_clauses (n_clauses/3) max_clauses e_p cnf;

    let rec main niv lit graphe interpretation =
        let interpretation = Array.copy interpretation in
        let graphe = ref graphe in
        let var = abs lit in interpretation.(var) <- lit;

        if propagation_unitaire [var] temoignage niv graphe !cnf interpretation
            then begin
                let var = branchement_th e_th n_var interpretation in
                if var > n_var
                    then SAT
                    else let lit = polarisation_naive var in
                    	match main (niv+1) lit ((niv+1,lit,-1)::!graphe) interpretation with
                    	    |SAT -> SAT
                    	    |Backjump (uip, c, l) ->
                    	        if l = niv
                    	            then main niv (-uip) ((niv,-uip,c)::!graphe) interpretation
                                    else Backjump (uip, c, l)
                            |UNSAT -> UNSAT
            end else begin
                let backjump = analyse !position e_th !e_p !graphe !cnf temoignage in

                while !position < !max_clauses && !cnf.(!position) <> [] do
                    incr position
                done;

                let moyenne = ref 0. in
                for c = n_clauses to !max_clauses-1 do
                    !e_p.(c) <- !e_p.(c) *. 0.995;
                    moyenne := !moyenne +. !e_p.(c)
                done;
                moyenne := !moyenne /. float_of_int (!max_clauses - n_clauses);

            	if !position >= !max_clauses
                    then begin
                        prolonger !max_clauses (!max_clauses/10) max_clauses e_p cnf;
                        position := nettoyage e_th !e_p !moyenne n_var n_clauses !max_clauses !cnf temoignage;
                        max_clauses := (Array.length !cnf)
                    end;

                backjump
            end
    in
    SAT = (main 0 0 [] interpretation)
;;
