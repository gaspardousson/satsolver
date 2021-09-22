%token <int> INT
%token NOT
%token CONJ DISJ
%token LPAR RPAR
%token EOF

%left CONJ DISJ
%nonassoc PREC_NOT

%start main
%type <Language.plf> main
%%

main :
    |expr EOF { $1 }
;

expr :
    |INT { Var $1 }
    |NOT expr %prec PREC_NOT { Not $2 }
    |LPAR expr CONJ expr RPAR { And ($2, $4) }
    |LPAR expr DISJ expr RPAR { Or ($2, $4) }
    |LPAR expr RPAR { $2 }
;