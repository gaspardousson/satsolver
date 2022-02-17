%token PB
%token EOC
%token EOCNF
%token <int> INT
%token EOF

%start main
%type <int * int * int list list> main
%%

main :
    PB INT INT
    cnf { $2, $3, $4 }
;

cnf :
    |clause cnf { $1::$2 }
    |EOF { [] }

clause :
    |INT clause { $1::$2 }
    |EOC { [] }
;