(* TODO: Improve naive solver *)

let naive_solver formula n_var =

    let rec verify formula assignment literal =
        match assignment with
        |[] ->  let satisfied = ref true in
                for i = 0 to Array.length formula - 1 do
                    satisfied := !satisfied && (formula.(i) = [||])
                done; !satisfied
        |head::tail-> for i = 0 to Array.length formula - 1 do
                let j = ref (Array.length formula.(i) - 1) in
                    while !j >= 0 do
                        if (head && formula.(i).(!j) = literal) || (not head && formula.(i).(!j) = -literal)
                            then begin
                                formula.(i) <- [||];
                                j := -1
                            end;
                        decr j
                    done;
                done;
                verify formula tail (literal+1)
    in

    let rec go_through formula n_var assignment =
        match n_var with
            |0 -> verify (Array.copy formula) assignment 1
            |_ -> go_through formula (n_var - 1) (true::assignment) || go_through formula (n_var - 1) (false::assignment)

    in

    go_through formula n_var []
;;


(* TODO: Improve quine solver *)

let quine_solver formula n_var =

    let remove array k =
        let length = Array.length array in
        let new_array = Array.make (length-1) array.(0) in

        let i = ref 0 in
        for j = 0 to length-1 do
            if j <> k
                then begin
                    new_array.(!i) <- array.(j);
                    incr i
                end
        done;

        new_array
    in

    let simplify formula literal =
        let simplified = ref (Array.copy formula) in
        let closed = ref false in

        for i = Array.length formula - 1 downto 0 do
            for j = Array.length formula.(i) - 1 downto 0 do

                if formula.(i).(j) = literal
                    then !simplified.(i) <- remove !simplified.(i) j;

                if formula.(i).(j) = -literal
                    then simplified := remove !simplified i;

            done;

            closed := !closed || (formula.(i) = [||])
        done;

        !simplified, !closed
    in

    let rec go_through formula n_var literal =
        match n_var with
            |0 -> true
            |_ -> let simplified, closed = simplify formula literal in
                    if closed
                        then false
                        else let next_lit = abs literal + 1
                             in go_through simplified (n_var - 1) next_lit || go_through simplified (n_var - 1) (-next_lit)
    in

    go_through formula n_var 0
;;

(* TODO: Heuristics *)



(* TODO: DPLL solver *)



(* TODO: Unit propagation *)



(* TODO: Two watched literals *)



(* TODO: CDCL solver *)


