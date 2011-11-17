(* CSCI 5535
 *
 * Implementation of CBV Lambda Calculus from Reynold's
 * Definitional Interpreters.
 *
 *)

open Printf 

type var  = string

type const = Int of int | Bool of bool |

type opr = 
  | Add 
  | Sub
  | Mult
  | Equal

type builtIn = 
  | Cons 

type exp =
  | Const of const
  | Var of var
  | Appl of exp * exp
  | Lambda of var * exp
  | Cond of exp * exp * exp
  | Letrec of stmt * exp
  | Pfk of opr * exp * exp
  | Cnk of builtIn * exp list 

and stmt = Bind of var * exp | Par of stmt * stmt

type value =
  | Integer of int
  | Boolean of bool
  | Funval of var * exp
  | CnkVal of se list

and se = 
  | Value of value
  | Ident of var  

let string_of_const c =
  match c with
    | Int i -> sprintf "%d" i
    | Bool b -> if b then "true" else "false"

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
  | Letrec (x, (y,e0), e1) ->
      sprintf "let %s = (lambda %s. %s) in %s"
	x y (exp_to_str e0) (exp_to_str e1)

let rec val_to_str v =
  match v with
    | Integer i -> sprintf "%d" i
    | Boolean b -> if b then "true" else "false"
    | Funval f -> "function"
