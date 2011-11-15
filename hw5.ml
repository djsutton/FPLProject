
(* CSCI 5535 HW5
 * 
 * DJ Sutton
 *)

open Ast

exception ApplicationTypeError

exception ConditionalTypeError

let ext (z: var) (a: value) (e: var -> value)=
    (fun x -> if x = z then a else (e x))

let rec 
eval (r: exp) (e: var -> value) : value = match r with
    Const (Int n) -> Integer n
  | Const (Bool b) -> Boolean b
  | Var x -> e x
  | Appl (r0, r1) -> let v1 = (eval r1 e) in
    let v0 = (eval r0 e) in (match v0 with
        | Funval f0 -> f0 v1
        | _ -> raise ApplicationTypeError)
  | Lambda (var, exp) -> (evlambda var exp e)
  | Cond (prem, conc, altr) -> (match (eval prem e) with
      Boolean b -> (if b then eval conc e else eval altr e)
    | _-> raise ConditionalTypeError)
  | Letrec (dvar, (dexp_fp,dexp_body), body) ->
      let rec e' = (fun x ->
        if x = dvar 
        then evlambda dexp_fp dexp_body e'
        else e x)
      in eval body e'

and
evlambda (fp: var) (body: exp) (e: var -> value) : value = 
  Funval (fun (a: value) -> 
      eval body (ext fp a e))
