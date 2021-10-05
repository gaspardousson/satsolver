(* TODO: Improve naive solver *)

let naive_solver form n_va =

    let rec verify form asgn dpth =
        match asgn with
        |[] ->  let bool = ref true in
                for i = 0 to Array.length form - 1 do
                    bool := !bool && (form.(i) = [||])
                done; !bool
        |h::t-> for i = 0 to Array.length form - 1 do
                let j = ref (Array.length form.(i) - 1) in
                    while !j >= 0 do
                        if (h && form.(i).(!j) = dpth) || (not h && form.(i).(!j) = -dpth)
                            then begin
                                form.(i) <- [||];
                                j := -1
                            end;
                        decr j
                    done;
                done;
                verify form t (dpth+1)
    in

    let rec go_through n_va asgn =
        match n_va with
            |0 -> verify form asgn 0
            |_ -> go_through (n_va - 1) (true::asgn) || go_through (n_va - 1) (false::asgn)

    in

    go_through n_va []
;;


(* TODO: Improve quine solver *)

let quine_solver form n_va =

    let remove arry k =
        let n = Array.length arry in
        let nrry = Array.make (n-1) arry.(0) in

        let i = ref 0 in
        for j = 0 to n-1 do
            if j <> k
                then begin
                    nrry.(!i) <- arry.(j);
                    incr i
                end
        done;

        nrry
    in

    let simplify form ltrl =
        let s_fm = ref (Array.copy form) in
        let clsd = ref false in

        for i = Array.length form - 1 downto 0 do
            for j = Array.length form.(i) - 1 downto 0 do

                if form.(i).(j) = ltrl
                    then !s_fm.(i) <- remove !s_fm.(i) j;

                if form.(i).(j) = -ltrl
                    then s_fm := remove !s_fm i;

            done;

            clsd := !clsd || (form.(i) = [||])
        done;

        !s_fm, !clsd
    in

    let rec go_through form n_va ltrl =
        match n_va with
            |(-1)-> true
            |_ ->   let s_fm, clsd = simplify form ltrl in
                    if clsd
                        then false
                        else let n_lt = abs ltrl + 1
                             in go_through s_fm (n_va - 1) n_lt || go_through s_fm (n_va - 1) (-n_lt)
    in

    go_through form n_va 0
;;

(* TODO: Heuristics *)



(* TODO: DPLL solver *)



(* TODO: Unit propagation *)



(* TODO: Two watched literals *)



(* TODO: CDCL solver *)


