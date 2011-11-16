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
%token EQ_TOK
%token IF
%token THEN
%token ELSE
%token LETREC
%token IN
%token LPAREN
%token RPAREN  
%token LBRACE
%token RBRACE

%token EOF

%start exp
%type <Ast.exp> exp

%left EQ_TOK


%%

exp : 
  app                                        { $1 }
| lambda                                     { Lambda (fst $1, snd $1) }
| COND LPAREN exp COMMA exp COMMA exp RPAREN { Cond($2,$4,$6) }
| LBRACE statement IN exp RBRACE             { Letrec($2, $4, $6) }
;
const : 
  INT                                        { Int $1 }
| TRUE                                       { Bool true }
| FALSE                                      { Bool false }
;
app:
  atom                                       { $1 }
| app atom                                   { Appl ($1,$2) }
;
atom: 
  const                                      { Const($1) }
| IDENTIFIER                                 { Var($1) }
| LPAREN exp RPAREN                          { $2 } 
;
lambda : LAMBDA IDENTIFIER DOT exp           { ($2,$4) }
;
statement : /*empty*/                        { [] }
| IDENTIFIER EQ exp                          { }
| statement STMTSEP statement                { }
;
