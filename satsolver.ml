type cnf = int array array;;



(* Naive solver *)

let verify form asgn =

    let rec simplify form asgn dpth =
        match asgn with

            |[] -> let bool = ref true in
                    for i = 0 to Array.length form - 1 do
                        bool := !bool && (form.(i) = [||])
                    done; !bool

            |h::t -> for i = 0 to Array.length form - 1 do
                        let j = ref (Array.length form.(i) - 1) in
                        while !j >= 0 do
                            if (h && form.(i).(!j) = dpth) || (not h && form.(i).(!j) = -dpth)
                                then begin
                                    form.(i) <- [||];
                                    j := -1
                                end;
                            decr j
                        done;
                    done; simplify form t (dpth+1)
    in

    simplify form asgn 0
;;



let naive_solver form n_va =

    let rec aux n_va asgn =
        match n_va with
            |0 -> verify form asgn
            |_ -> aux (n_va - 1) (true::asgn) || aux (n_va - 1) (false::asgn)

    in aux n_va []
;;