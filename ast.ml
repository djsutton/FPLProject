(* CSCI 5535
 *
 * Implementation of the syntax tree for parallel lambda 
 * calculus 
 *
 * By DJ Sutton and Matt Ripley
 *)

open Printf 

(* variable type *)
type var  = string

(* constant type *) 
type const = Int of int | Bool of bool

(* primitive operator type *)
type opr = 
  | Add 
  | Sub
  | Mult
  | Equal

(* Built in functions *)
type builtIn = 
  | Cons (* Constructor operator. Constructs list out of CSV's *)

(* expression type *)
type exp =
  | Const of const
  | Var of var
  | Appl of exp * exp
  | Lambda of var * exp
  | Cond of exp * exp * exp
  | Letrec of stmt * exp
  | Pfk of opr * exp * exp
  | Cnk of builtIn * exp list 

(* Statment type *)
and stmt = Bind of var * exp | Par of stmt * stmt

(* Value types as defined in the paper *)
type value =
  | Integer of int
  | Boolean of bool
  | Funval of var * exp
  | CnkVal of se list
(* "Simple" expressions as defined in the paper *)
and se = 
  | Value of value
  | Ident of var  

(* Functions to convert AST into a string *)

let string_of_const c =
  match c with
    | Int i -> sprintf "%d" i
    | Bool b -> if b then "true" else "false"

let builtIn_to_str c = match c with
  | Cons -> "Cons"

let opr_to_str op =
    match op with
      | Add -> "+"
      | Sub -> "-"
      | Mult -> "*"
      | Equal -> "="

let rec exp_to_str e = 
  match e with
  | Const(n) -> string_of_const n
  | Var(l) -> l
  | Appl(e0,e1) ->
      sprintf "(%s %s)" (exp_to_str e0) (exp_to_str e1)
  | Lambda(x,e) ->
      sprintf "(lambda %s. %s)" x (exp_to_str e)
  | Cond(e0,e1,e2) ->
      sprintf "if %s then %s else %s"
	(exp_to_str e0) (exp_to_str e1) (exp_to_str e2)
  | Letrec (s,e) ->
      sprintf "{%s in %s}" (stmt_to_str s) (exp_to_str e)
  | Pfk(op, e1, e2) ->
    sprintf "%s(%s, %s)" (opr_to_str op) (exp_to_str e1) (exp_to_str e2)
  | Cnk(bi, expList) -> 
    sprintf "%s(%s)" (builtIn_to_str bi) (expList_to_str expList)
    
and expList_to_str expList = 
  (String.concat "," (List.map exp_to_str expList))

and stmt_to_str s = match s with
  | Bind(v,e) -> sprintf "%s = %s" v (exp_to_str e)
  | Par(s0,s1) -> sprintf "%s ; %s" (stmt_to_str s0) (stmt_to_str s1)

let rec val_to_str v =
  match v with
    | Integer i -> sprintf "%d" i
    | Boolean b -> if b then "true" else "false"
    | Funval (v,e) -> "function"
