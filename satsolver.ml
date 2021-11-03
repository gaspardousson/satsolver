(* Useful libraries *)

open Int;;



(* Naive solver *)

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



(* to_propagate: DPLL solver *)

let dpll_solver problem =
    let n_var, formula = problem in
    let n = Array.length formula in
    let watchers = Array.make (n_var+1) [] in
    let to_propagate = ref [] in

    (* propagation *)
    let rec search_new_watcher clause assignment =
        match clause with
            |[] -> 0, []
            |lit::queue -> if assignment.(abs lit) = -lit
                                then let w, c = search_new_watcher queue assignment in
                                    w, lit::c
                                else lit, queue
    in

    let propagate_clause c var assignment =
        match formula.(c) with
            |[] -> failwith "Empty clause"
            |[lit] -> failwith "Unit clause"
            |lit1::lit2::queue -> let var1, var2 = abs lit1, abs lit2 in
                                    match assignment.(var1), assignment.(var2) with
                                        |asgn1, asgn2 when asgn1 = lit1 || asgn2 = lit2 ->
                                            true, true
                                        |0, asgn2 ->
                                            let w, q = search_new_watcher queue assignment in
                                            if w = 0
                                                then (assignment.(var1) <- lit1; to_propagate := var1::!to_propagate; true, true)
                                                else (let v = abs w in watchers.(v) <- c::watchers.(v);
                                                    formula.(c) <- lit1::w::lit2::q; true, false);
                                        |asgn1, 0 ->
                                            let w, q = search_new_watcher queue assignment in
                                            if w = 0
                                                then (assignment.(var2) <- lit2; to_propagate := var2::!to_propagate; true, true)
                                                else (let v = abs w in watchers.(v) <- c::watchers.(v);
                                                    formula.(c) <- w::lit2::lit1::q; true, false);
                                        |_ ->
                                            let w, q = search_new_watcher queue assignment in
                                            if w = 0
                                                then false, true
                                            else begin
                                            	    let v = abs w in assignment.(v) <- w;
                                            	    watchers.(v) <- c::watchers.(v);
                                            	    formula.(c) <- if var1 = var then w::lit2::lit1::q else lit1::w::lit2::q;
                                            	    to_propagate := v::!to_propagate; true, false
                                            	end
    in

    let rec propagate_variable watching var assignment =
        match watching with
            |[] -> true, []
            |clause::queue -> let clause_propagated, watching_it = propagate_clause clause var assignment in
                              if clause_propagated
                              then let propagated, still_watching = propagate_variable queue var assignment in
                                    propagated, if watching_it then clause::still_watching else still_watching
                              else false, watching
    in

    let rec propagate assignment =
        match !to_propagate with
            |[] -> true
            |var::queue -> to_propagate := queue;
                           let propagated, still_watching = propagate_variable watchers.(var) var assignment in
                           watchers.(var) <- still_watching;
                           propagated && propagate assignment
    in

    (* main *)
    let rec main assignment =
        let asgn = Array.copy assignment in
        if propagate asgn
            then begin
                let var = ref 1 in
                while !var <= n_var && asgn.(!var) <> 0 do
                    incr var
                done;
                (!var > n_var) ||
                (asgn.(!var) <- !var; to_propagate := !var::!to_propagate; main asgn) ||
                (asgn.(!var) <- - !var; to_propagate := !var::!to_propagate; main asgn)
            end
            else begin
                to_propagate := [];
                false
            end
    in

    (* init *)
    let assignment = Array.make (n_var+1) 0 in
    let empty_clause = ref false in
    for c = 0 to n-1 do
        let w1, q1 = search_new_watcher formula.(c) assignment in
        let v1 = abs w1 in
        if w1 = 0
            then empty_clause := true
            else watchers.(v1) <- c::watchers.(v1);
        let w2, q2 = search_new_watcher q1 assignment in
        let v2 = abs w2 in
        if w2 = 0
            then (assignment.(v1) <- w1; to_propagate := v1::!to_propagate)
            else watchers.(v2) <- c::watchers.(v2);
        if v1 = v2 then failwith "Repetition";
    done;

    main assignment
;;



(* to_propagate: CDCL solver *)

let cdcl_solver problem =
    ()
;;
