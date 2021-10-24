%token PB
%token EOC
%token EOCNF
%token <int> INT
%token EOF

%start main
%type <int * int list list> main
%%

main :
    PB INT INT
    cnf EOC EOF { $2, $4 }
;

cnf :
    |clause cnf { $1::$2 }
    |EOCNF { [] }

clause :
    |INT clause { $1::$2 }
    |EOC { [] }
;