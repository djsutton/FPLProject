(* CSCI 5535
 * 
 * Implementation of the reduction rules parallel lambda 
 * calculus 
 *
 * By DJ Sutton and Matt Ripley
 *)

open Ast

exception SubTypeError
exception ConditionalTypeError
exception EvalTypeError
exception PfkTypeError
exception NotImplemented
exception ReduceTypeError
exception ApplicationTypeError
exception FlattenTypeError
exception LiftTypeError

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
let rec sub (e : exp) (v : var) (r : exp) : exp = 
  match r with
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

let evalPfk (op: opr) expList : exp = 
  match (List.nth expList 0, List.nth expList 1) with 
    | (Const(Int(n)), Const(Int(m))) -> 
      (match op with 
    | Add -> Const(Int(n+m))
    | Sub -> Const(Int(n-m))
    | Mult -> Const(Int(n*m))
    | Intdiv -> Const(Int(n/m))
    | Modulo -> Const(Int(n mod m))
    | Equal -> Const(Bool(n=m))
      ) 
    | (Const(Bool(b1)), Const(Bool(b2))) ->
      (match op with
    | Equal -> Const(Bool(b1 = b2))
    | _ -> raise PfkTypeError
      )
    | (Const(Bool(b1)), Const(Int(b2))) -> raise PfkTypeError
    | (Const(Int(b1)), Const(Bool(b2))) -> raise PfkTypeError
    | (_, _) -> Pfk(op, expList)

let rec alphaRenameStmt (v:var) (nv:var) (s:stmt) : stmt = 
  match s with 
    | Bind (v1, e1) -> 
      if v1 = v then
    Bind(nv, alphaRename v nv e1)
      else
    Bind(v1, alphaRename v nv e1)
    | Par (s1, s2) -> 
      let s1' = alphaRenameStmt v nv s1 in
      let s2' = alphaRenameStmt v nv s2 in 
      Par(s1', s2')

and alphaRename (v:var) (nv:var) (e:exp) : exp=  
  match e with 
    | Const(n) -> e
    | Var(x) -> 
      if x = v then 
    Var(nv)
      else
    e

    | Appl(e1, e2) -> Appl( (alphaRename v nv e1), (alphaRename v nv e2))
    | Lambda(v1, body) -> 
      let b = alphaRename v nv body in 
      if v1 = v then 
    Lambda(nv, b)
      else
    Lambda(v1, b)
    
    | Cond(bVal, tExp, fExp) ->
      let nbVal = alphaRename v nv bVal in
      let ntExp = alphaRename v nv tExp in 
      let nfExpt = alphaRename v nv fExp in 
      Cond(nbVal, ntExp, nfExpt)

    | Letrec(s, e1) ->
      let s' = alphaRenameStmt v nv s in 
      let e1' = alphaRename v nv e1 in 
      Letrec(s', e1')

    | Pfk(op, expList) -> 
      let expList' = List.map (alphaRename v nv) expList in
      Pfk(op, expList')
    | Cnk(bIn, expList) -> 
      Cnk(bIn, List.map (alphaRename v nv) expList)

let mangle (e:exp) (v:var) : exp = 
  let newVar =  ( (fst v),  (snd v) +1 ) in 
  alphaRename v newVar e

let mangleStmt (s:stmt) (v:var) : stmt = 
   let newVar =  ( (fst v),  (snd v) +1 ) in 
   alphaRenameStmt v newVar s

let rec getVars (s:stmt): var list =
  match s with
  | Bind(x, e) -> [x]
  | Par(s1, s2) -> (getVars s1) @ (getVars s2)

let rec flatten (b:stmt) (vars:var list): stmt = 
  match b with 
    | Bind(x, Letrec(s, e)) ->
      let e' = List.fold_left mangle e vars in
      let s' = List.fold_left mangleStmt s vars in 
      Par(Bind(x,e'), s')
    | _ -> raise FlattenTypeError

let rec lift (e:exp) : exp = 
  let tVar = ("t",0) in
  match e with 
    | Letrec(s, e1) ->
      let s' = mangleStmt s tVar in 
      let e1' = mangle e1 tVar in 
      Letrec(Par(s', Bind(tVar, e1')), Var(tVar))
    | Appl(e1, e2) ->
      let e1' = mangle e1 tVar in 
      let e2' = mangle e2 tVar in 
      if isSimp e1' then 
        Letrec(Bind(tVar, e2'), Appl(e1', Var(tVar)))
      else
        Letrec(Bind(tVar, e1'), Appl(Var(tVar), e2'))
    | Cond(bVal, tExp, fExp) -> 
      let bVal' = mangle bVal tVar in
      let tExp' = mangle tExp tVar in 
      let fExp' = mangle fExp tVar in
      Letrec(Bind(tVar, bVal'), Cond(Var(tVar), tExp', fExp'))
    | _ -> raise LiftTypeError

let rec reduce (n:int) (e:exp) : exp = 
  if isSimp e || n = 0 then 
    e 
  else
    match e with 
      | Cond(cond, tExp, fExp) -> 
        let c = reduce (n-1) cond in 
        (match c with
          | Const(Bool(b)) -> 
            if b then 
                reduce (n-1) tExp 
            else
                reduce (n-1) fExp
          | _ -> raise ConditionalTypeError
        )
      | Appl(e1, e2) -> 
        (match e1 with
          | Lambda(var, body) -> Letrec(Bind(var,e2),e1)
          | _ -> raise ApplicationTypeError
        )
      | Cnk(bIn, expList) -> 
        Cnk(bIn, List.map (reduce (n-1)) expList)
      | Pfk(op, expList) -> 
        let expList' = List.map (reduce (n-1)) expList in 
        evalPfk op expList'
      | Letrec(s, body) -> raise NotImplemented
      | _ -> raise ReduceTypeError


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
