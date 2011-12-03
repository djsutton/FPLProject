(* CSCI 5535 HW5
 * 
 * DJ Sutton
 *)

open Ast

exception SubTypeError

exception ConditionalTypeError

exception EvalTypeError

(* Checks to see if a list of expressions are all simple expression types *) 
let rec isSimp (e:exp) : bool =
  match e with 
    | Const(n) -> true
    | Var(x) -> true
    | Lambda(v, e1) -> true
    | Cnk(b, eList) -> 
      List.fold_left ( fun b e1 -> b && isSimp(e1) ) true eList
    | _ -> false

(* substitute e for v in R *)
let rec sub (e : exp) (v : var) (r : exp) : exp = match r with
  | Const n -> r
  | Var x -> if x = v then e else r
  | Appl (e1, e2) -> Appl(sub e v e1, sub e v e2)
  | Lambda (x, e1) -> if x = v then r else Lambda (x, sub e v e1)
  | Cond (prem, conc, altr) ->
        Cond (sub e v prem, sub e v conc, sub e v altr)
  | Letrec(s, e1) -> Letrec(sub_stmt e v s, sub e v e1)
  | _ -> raise SubTypeError

and sub_stmt (e : exp) (v : var) (s : stmt) : stmt = match s with
  | Bind(vName, e1) -> if vName = v then s else Bind(vName, e) 
  | Par(s1, s2) -> Par(sub_stmt e v s1, sub_stmt e v s2)


(*let rec eval (r: exp) : value = match r with
    Const (Int n) -> Integer n
  | Const (Bool b) -> Boolean b
  | Var x -> e x
  | Appl (r0, r1) -> let v1 = (eval r1 e) in
    let v0 = (eval r0 e) in (match v0 with
        | _ -> raise ApplicationTypeError)
  | Cond (prem, conc, altr) -> (match (eval prem e) with
      Boolean b -> (if b then eval conc e else eval altr e)
    | _-> raise ConditionalTypeError)
  | _ -> raise EvalTypeError *)
