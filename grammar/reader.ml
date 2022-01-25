open Parser;;

let lire_cnf chemin =

    let lire_fichier chemin =
        let fichier = open_in chemin in
        let texte = really_input_string fichier (in_channel_length fichier) in
        close_in fichier;
        texte
    in

    let n_var, n_clauses, cnf = Parser.main Lexer.read_token (Lexing.from_string (lire_fichier chemin))
    in n_var, n_clauses, Array.of_list cnf
;;
