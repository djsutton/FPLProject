(* CSCI 5535
 * 
 * Implementation of the reduction rules parallel lambda 
 * calculus 
 *
 * By DJ Sutton and Matt Ripley
 *)

open Ast
open Printf

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

(* Eval PFK terms and primitive operators. Valid combinations for *
 * types are INT,INT or BOOL, BOOL. Operators other than equals   *
 * are not implemented for boolean types *)
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

(* Alpha renaming for the statement type 
* Replace all v with nv in s *) 
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

(*  Alpha renaming for exp type.
 * Replace all vars v with nv in expression e *) 
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

(* Generate a new name for a variable for alpha renaming 
 * In this case we increment the numeric identifier in 
 * the touple by 1 to generate a unique name  *)
let mangle (e:exp) (v:var) : exp = 
  let newVar =  ( (fst v),  (snd v) +1 ) in 
  alphaRename v newVar e

(* Generate a new name for a variable in a stmt type 
 * Uses same technique as the regular mangle function *)
let mangleStmt (s:stmt) (v:var) : stmt = 
   let newVar =  ( (fst v),  (snd v) +1 ) in 
   alphaRenameStmt v newVar s

(* Get all vars in a statment s *) 
let rec getVars (s:stmt): var list =
  match s with
  | Bind(x, e) -> [x]
  | Par(s1, s2) -> (getVars s1) @ (getVars s2)

(* substitute e for v in r *)
let rec sub (e : exp) (v : var) (r : exp) : exp = 
  match r with
  | Const n -> r
  | Var x -> if x = v then e else r
  | Appl (e1, e2) -> Appl(sub e v e1, sub e v e2)
  | Lambda (x, e1) -> if x = v then r else Lambda (x, sub e v e1)
  | Cond (prem, conc, altr) ->
        Cond (sub e v prem, sub e v conc, sub e v altr)
  | Pfk(op, expList) -> Pfk(op, List.map (sub e v) expList)
  | Cnk(c, expList) -> Cnk(c, List.map (sub e v) expList)
  | Letrec(s, e1) -> 
    if List.mem v (getVars s) then
        r
    else
        Letrec(sub_stmt e v s, sub e v e1)

(* Substitute for stmt type *) 
and sub_stmt (e : exp) (v : var) (s : stmt) : stmt = match s with
  | Bind(vName, e1) -> Bind(vName, (sub e v e1)) 
  | Par(s1, s2) -> Par(sub_stmt e v s1, sub_stmt e v s2)

(* Flatten function. Generates a new stmt with all name mangling
 * completed *)
let rec flatten (b:stmt) (vars:var list): stmt = 
  match b with 
    | Bind(x, Letrec(s, e)) ->
      let e' = List.fold_left mangle e vars in
      let s' = List.fold_left mangleStmt s vars in 
      Par(Bind(x,e'), s')
    | _ -> raise FlattenTypeError

(* Lifts out a expression and stuff it into a let rec with 
 * a mangled identifier *) 
let rec lift (e:exp) : exp = 
  let tVar = ("t",0) in
  match e with 
    | Letrec(s, e1) ->
      if not (isSimp e1) then
        let s' = mangleStmt s tVar in 
        let e1' = mangle e1 tVar in 
        Letrec(Par(s', Bind(tVar, e1')), Var(tVar))
      else
        e
    | Appl(e1, e2) ->
      let e1' = mangle e1 tVar in 
      let e2' = mangle e2 tVar in 
      if not (isSimp e1) then
        Letrec(Bind(tVar, e2'), Appl(e1', Var(tVar)))
      else if not (isSimp e2) then
        Letrec(Bind(tVar, e1'), Appl(Var(tVar), e2'))
      else
        e
    | Cond(bVal, tExp, fExp) ->
      if not (isSimp(bVal)) then
          let bVal' = mangle bVal tVar in
          let tExp' = mangle tExp tVar in 
          let fExp' = mangle fExp tVar in
          Letrec(Bind(tVar, bVal'), Cond(Var(tVar), tExp', fExp'))
      else
        e
    | _ -> e

(* Check to see if it is possible to subtitute a stmt*)
let rec check_sub (s:stmt) = 
  match s with
    | Bind(v, e) -> 
      if isSimp e then 
          [(v,e)]
      else
          []
    | Par(s1,s2) ->
        (check_sub s1)@(check_sub s2)

(* Check to see if we can flatten a stmt *)
let rec check_flatten (vList : var list) (s:stmt) =
  match s with
    | Bind(v, Letrec(stmnt, expr)) -> [flatten s vList]
    | Par(s1, s2) -> 
        let left = check_flatten vList s1 in
        if List.length left > 0 then
            [Par(List.hd left, s2)]
        else
            let right = check_flatten vList s2 in
            if List.length right > 0 then
                [Par(s1, List.hd right)]
            else
                []
    | _ -> []

let rec check_lift (s:stmt) : stmt = 
  match s with 
    | Bind(v, e) -> 
      if not(isSimp e) then
    let e' = lift e in
    Bind(v, e')
      else
    s
    | Par(s1, s2) -> 
      let left = check_lift s1 in 
      if left <> s1 then
    Par(left, s2)
      else
    Par(s1, check_lift s2)

let rec sub_once sub_list (body:exp) = 
    if List.length sub_list > 0 then 
        let v,e = List.hd sub_list in
        let body' = sub e v body in
        if body' = body then
            sub_once (List.tl sub_list) body
        else
            body'
    else
        body

(* Reduce a letrec first see if we can do any substitutions. If we 
 * can't then try to flatten. Failing all of this do ... *)
let reduce_letrec (s:stmt) (body:exp) : exp = 
  let sub_list = check_sub s in
  let body' = sub_once sub_list body in
  if body' != body then
    Letrec(s, body')
  else
    let check = check_flatten (getVars s) s in
    if List.length check > 0 then
      Letrec(List.hd check, body)
    else
      let s' = check_lift s in 
        if s' <> s then
          Letrec(s', body)
        else
          Letrec(s, body)

(* Reduce the expression n number of steps. When n is reached
 * we just stop evaluating *) 
let rec reduce (n:int) (e:exp) : exp*int =
  if isSimp e || n = 0 then 
    e,n
  else
    match e with 
      | Cond(cond, tExp, fExp) -> 
        let c,count = reduce (n-1) cond in 
        (match c with
          | Const(Bool(b)) -> 
            if b then 
                reduce (n-1) tExp 
            else
                reduce (n-1) fExp
          | _ -> e,n
        )
      | Appl(e1, e2) -> 
        let e1',_ = reduce (n-1) e1 in
        let e2',_ = reduce (n-1) e2 in
        (match e1' with
          | Lambda(var, body) -> reduce (n-1) (Letrec(Bind(var,e2'),body))
          | _ -> Appl(e1',e2'), n
        )
      | Cnk(bIn, expList) -> 
        Cnk(bIn, (List.map fst (List.map (reduce (n-1)) expList))),(n-1)
      | Pfk(op, expList) -> 
        let expList' = List.map fst (List.map (reduce (n-1)) expList) in 
        (evalPfk op expList'),(n-1)
      | Letrec(s, body) -> 
        let body',count = reduce n body in
        if body'<>body then
            reduce count (Letrec(s, body'))
        else
            let newLetRec = reduce_letrec s body in
            if newLetRec <> e then
                reduce (n-1) newLetRec
            else
                e,n
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
