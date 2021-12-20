open Parser;;

let read_cnf path =

    let read_whole_file path =
        let file = open_in path in
        let s = really_input_string file (in_channel_length file) in
        close_in file;
        s
    in

    let n_var, n_cla, cnf = Parser.main Lexer.read_token (Lexing.from_string (read_whole_file path))
    in n_var, n_cla, Array.of_list cnf
;;
