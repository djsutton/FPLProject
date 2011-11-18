
(* CSCI 5535 HW5
 * 
 * DJ Sutton
 *)

open Ast

exception ApplicationTypeError

exception ConditionalTypeError

exception EvalTypeError

let ext (z: var) (a: value) (e: var -> value)=
    (fun x -> if x = z then a else (e x))

let rec 
eval (r: exp) (e: var -> value) : value = match r with
    Const (Int n) -> Integer n
  | Const (Bool b) -> Boolean b
  | Var x -> e x
  | Appl (r0, r1) -> let v1 = (eval r1 e) in
    let v0 = (eval r0 e) in (match v0 with
        | _ -> raise ApplicationTypeError)
  | Cond (prem, conc, altr) -> (match (eval prem e) with
      Boolean b -> (if b then eval conc e else eval altr e)
    | _-> raise ConditionalTypeError)
  | _ -> raise EvalTypeError
