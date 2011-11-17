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
%token IN
%token LPAREN
%token RPAREN  
%token LBRACE
%token RBRACE
%token COMMA
%token CONS
%token PLUS
%token MINUS
%token MULT
%token COMMA

%token EOF

%start exp
%type <Ast.exp> exp

%left EQ


%%

exp : 
  IDENTIFIER                                 { Var($1) }
| LPAREN exp RPAREN                          { $2 }
| exp exp                                    { Appl ($1,$2) }
| LAMBDA IDENTIFIER DOT exp                  { Lambda($2,$4) }
| LBRACE statement IN exp RBRACE             { Letrec($2,$4) }
| COND LPAREN exp COMMA exp COMMA exp RPAREN { Cond($2,$4,$6) }
| pfk                                        { $1 }
| const                                      { Const($1) }
| cnk                                        { Cnk($)}
;

statement : /*empty*/                        { [] }
| IDENTIFIER EQ exp                          { Bind($1,$3) }
| statement STMTSEP statement                { Par($1,$3) }
;

pfk:
  PLUS LPAREN exp COMMA exp RPAREN           { Pfk(Add,$3,$5)}
| MINUS LPAREN exp COMMA exp RPAREN          { Pfk(Sub,$3,$5)}
| MULT LPAREN exp COMMA exp RPAREN           { Pfk(Mult,$3,$5)}
;

const : 
  INT                                        { Int $1 }
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
