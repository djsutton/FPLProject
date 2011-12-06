%{
(* CSCI 5535
 * 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html
 * but basically it works just like Yacc/Bison.
 * See http://en.wikipedia.org/wiki/YACC
 *)

open Ast		    (* abstract syntax *)

let error msg	= failwith msg

%}

%token <string>         IDENTIFIER
%token <int>            INT

%token LAMBDA
%token DOT
%token TRUE
%token FALSE
%token EQ
%token IF
%token THEN
%token ELSE
%token LETREC
%token COND
%token IN
%token LPAREN
%token RPAREN  
%token LBRACE
%token RBRACE
%token COMMA
%token CONS
%token COMMA
%token STMTSEP
%token PLUS
%token MINUS
%token MULT
%token INTDIV
%token MODULO

%token EOF

%start exp
%type <Ast.exp> exp

%left STMTSEP
%left ELSE
%left EQ
%left PLUS MINUS
%left MULT INTDIV MODULO
%nonassoc IDENTIFIER INT LAMBDA TRUE FALSE IF COND LPAREN LBRACE CONS DOT
%left apply


%%

exp : 
  IDENTIFIER                                    { Var(($1, 0)) }
| LPAREN exp RPAREN                             { $2 }
| exp exp    %prec apply                        { Appl ($1,$2) }
| LAMBDA IDENTIFIER DOT exp                     { Lambda( ($2,0) ,$4) }
| LBRACE statement IN exp RBRACE                { Letrec($2,$4) }
| COND LPAREN exp COMMA exp COMMA exp RPAREN    { Cond($3,$5,$7) }
| IF exp THEN exp ELSE exp                      { Cond($2,$4,$6) }
| pfk                                           { $1 }
| const                                         { Const($1) }
| cnk                                           { $1 }
;

statement : 
  IDENTIFIER EQ exp                             { Bind( ($1,0) ,$3) }
| statement STMTSEP statement                   { Par($1,$3) }
;

pfk:
  PLUS LPAREN exp COMMA exp RPAREN              { Pfk(Add,[$3;$5])}
| exp PLUS exp                                  { Pfk(Add,[$1;$3])}
| MINUS LPAREN exp COMMA exp RPAREN             { Pfk(Sub,[$3;$5])}
| exp MINUS exp                                 { Pfk(Sub,[$1;$3])}
| MULT LPAREN exp COMMA exp RPAREN              { Pfk(Mult,[$3;$5])}
| exp MULT exp                                  { Pfk(Mult,[$1;$3])}
| EQ LPAREN exp COMMA exp RPAREN                { Pfk(Equal,[$3;$5])}
| exp EQ exp                                    { Pfk(Equal,[$1;$3])}
| INTDIV LPAREN exp COMMA exp RPAREN            { Pfk(Intdiv, [$3;$5])}
| exp INTDIV exp                                { Pfk(Intdiv, [$1;$3])}
| MODULO LPAREN exp COMMA exp RPAREN            { Pfk(Modulo, [$3;$5])}
| exp MODULO exp                                { Pfk(Modulo, [$1;$3])}
;

const : 
  INT                                        { Int $1 }
| MINUS INT                                  { Int (-$2) }
| TRUE                                       { Bool true }
| FALSE                                      { Bool false }
;

cnk:
| CONS LPAREN args RPAREN                    { Cnk(Cons,$3) }
;

args : /* empty */                           { [] }
| some_args                                  { $1 }
;

some_args:
| exp COMMA some_args                        { $1::$3 }
| exp                                        { [$1] }
;
