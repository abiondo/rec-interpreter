%token <Z.t> NUM
%token <string> IDENT

%token LET FN
%token ASSIGN IF THEN ELSE
%token LPAREN RPAREN
%token COMMA SEMI
%token PLUS MINUS TIMES
%token EOF

%nonassoc ELSE

%left PLUS MINUS
%left TIMES

%start program
%type <Language.var_def> var_def
%type <Language.func_def> func_def
%type <Language.program> program

%%

id_list:
                        { [] }
  | IDENT               { [$1] }
  | id_list COMMA IDENT { $1 @ [$3] }
  ;

term_list:
                         { [] }
  | term                 { [$1] }
  | term COMMA term_list { $1 :: $3 }
  ;

term:
    NUM                           { Language.Num($1) }
  | IDENT                         { Language.Var($1) }
  | term PLUS term                { Language.Add($1, $3) }
  | term MINUS term               { Language.Sub($1, $3) }
  | term TIMES term               { Language.Mul($1, $3) }
  | LPAREN term RPAREN            { $2 }
  | IF term THEN term ELSE term   { Language.Cond($2, $4, $6) }
  | IDENT LPAREN term_list RPAREN { Language.Call($1, $3) }
  ;

var_def: LET IDENT ASSIGN NUM
  { { name = $2; value = $4; } };

func_def: FN IDENT LPAREN id_list RPAREN ASSIGN term
  { { name = $2; params = $4; body = $7; } };

program: list(var_def SEMI {$1}) list(func_def SEMI {$1}) term EOF
  { { vars = $1; funcs = $2; term = $3; } };
