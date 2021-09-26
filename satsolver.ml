type cnf = int array array;;

let read_cnf path =
    let rec read_clause list depth =
        match list with
            |[] -> Array.make depth 0
            |["0"] -> Array.make depth 0
            |""::q -> read_clause q depth
            |t::q -> let clause = read_clause q (depth+1) in
                        clause.(depth) <- int_of_string t;
                        clause
    in
    let rec read_line file depth =
        let line = input_line file in
        match line.[0] with
            |'c' -> read_line file depth
            |'p' -> read_line file depth
            |'%' -> Array.make depth [||]
            |_ -> let cnf = read_line file (depth+1) in
                    cnf.(depth) <- read_clause (String.split_on_char ' ' line) 0;
                    cnf
    in
    read_line (open_in path) 0
;;


let eval form asgn =
    let rec disj i j =
        match j with
        |_ when j = Array.length form.(i) -> false
        |_ -> let literal = form.(i).(j) in
            (literal > 0 && asgn.(literal)) || (literal < 0 && not asgn.(-literal)) || disj i (j+1)
    in
    let rec conj i =
        match i with
        |_ when i = Array.length form -> true
        |_ -> (disj i 0) && (conj (i+1))
    in
    conj 0
;;

let naive_solver form =
    let n = (Array.length form) + 1 in
    let asgn = Array.make n false in
    let rec next conf =
        let i = ref 1 in
        while !i < n && conf.(!i) do
            conf.(!i) <- false;
            incr i;
        done;
        if !i < n then conf.(!i) <- true;
        !i < n
    in
    let rec test conf =
        match eval form conf with
            |true -> true
            |false -> if next conf then test conf else false
    in
    asgn.(0) <- test asgn; asgn
;;