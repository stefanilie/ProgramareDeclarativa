open Mem
open ImpAST

let is_fun = function
  | Fun _ | IntOfFloat _ | FloatOfInt _ -> true
  | _ -> false

let is_val = function
  | Bool _ | Int _ | Float _ | Skip _ -> true
  | e -> is_fun e

let rec reduce = function
  | (Op(Int (n1,_),Plus,Int (n2,_),loc),s) -> Some (Int (n1+n2,loc),s)             (*Op+*)
  | (Op(Float (f1,_),Plus,Float (f2,_),loc),s) -> Some (Float (f1+.f2,loc),s)
  | (Op(Int (n1,_),Minus,Int (n2,_),loc),s) -> Some (Int (n1-n2,loc),s)             (*Op+*)
  | (Op(Float (f1,_),Minus,Float (f2,_),loc),s) -> Some (Float (f1-.f2,loc),s)
  | (Op(Int (n1,_),Mul,Int (n2,_),loc),s) -> Some (Int (n1*n2,loc),s)             (*Op+*)
  | (Op(Float (f1,_),Mul,Float (f2,_),loc),s) -> Some (Float (f1*.f2,loc),s)
  | (Op(Int (n1,_),Div,Int (n2,_),loc),s) when n2 <> 0 -> Some (Int (n1/n2,loc),s)             (*Op+*)
  | (Op(Float (f1,_),Div,Float (f2,_),loc),s) -> Some (Float (f1/.f2,loc),s)

  | (Op(Int (n1,_),Mic,Int (n2,_),loc),s) -> Some (Bool (n1<=n2,loc),s)            (*Op<=*)
  | (Op(Float (f1,_),Mic,Float (f2,_),loc),s) -> Some (Bool (f1<=f2,loc),s)            (*Op<=*)
  | (Op(Int (n1,_),MicS,Int (n2,_),loc),s) -> Some (Bool (n1<n2,loc),s)
  | (Op(Float (f1,_),MicS,Float (f2,_),loc),s) -> Some (Bool (f1<f2,loc),s)
  | (Op(Int (n1,loc1),op,e2,loc),s) ->                                        (*OpD*)
    (match reduce (e2,s) with 
      | Some (e2',s') -> Some (Op(Int (n1,loc1),op,e2',loc),s')
      | None -> None
    )
  | (Op(Float (f1,loc1),op,e2,loc),s) ->                                        (*OpD*)
    (match reduce (e2,s) with 
      | Some (e2',s') -> Some (Op(Float (f1,loc1),op,e2',loc),s')
      | None -> None
    )
  | (Op(e1,op,e2,loc),s) ->                                            (*OpS*)
    (match reduce (e1,s) with Some (e1',s') -> Some (Op(e1',op,e2,loc),s')
      | None -> None)
  | (Loc (l,loc), s) -> Some (lookup l s, s)                    (*Loc*)
  | (Atrib(l, Float (f,loc),loc'),s) ->                                         (*Atrib*)
      Some (Skip loc', update (l, Float (f, loc)) s)
  | (Atrib(l, Int (n,loc),loc'),s) ->                                         (*Atrib*)
      Some (Skip loc', update (l, Int (n, loc)) s)
  | (Atrib(l,e,loc),s) ->                                          (*AtribD*)
    (match reduce (e,s) with Some (e',s') -> Some (Atrib(l,e',loc),s')
      | None -> None)
  | (Secv(Skip _,e,_),s) -> Some (e,s)                                 (*Secv*)
  | (Secv(e1,e2,loc),s) ->                                             (*SecvS*)
    (match reduce (e1,s) with Some (e1',s') -> Some (Secv(e1',e2,loc),s')
      | None -> None)
  | (If(Bool (true,_),e1,e2,_),s) -> Some (e1,s)                         (*IfTrue*)
  | (If(Bool (false,_),e1,e2,_),s) -> Some (e2,s)                        (*IfFalse*)
  | (If(e,e1,e2,loc),s) ->                                             (*IfS*)
    (match reduce (e,s) with Some (e',s') -> Some (If(e',e1,e2,loc),s')
      | None -> None)
  | (While(e1,e2,loc),s) -> Some (If(e1,Secv(e2,While(e1,e2,loc),loc),Skip loc,loc),s) (*While*)
  | (For(init,cond,incr,body,l), s) 
    -> Some (Secv(init,While(cond,Secv(body,incr,l),l),l), s)    (*For*)
  | (App (IntOfFloat _, Float (f,_), loc), s)
    -> Some (Int (int_of_float f, loc), s)
  | (App (FloatOfInt _, Int (n,_), loc), s)
    -> Some (Float (float_of_int n, loc), s)
  | (App (App(Z loc, g, loc1), v, loc2), s)
    -> Some (App (App (g, App(Z loc, g, loc),loc1), v, loc2), s)
  | (App (Fun(x,_,e1,_),e2,_),s) when is_val e2 -> Some (subst x e2 e1, s)
  | (App (e1, e2, loc), s) when is_fun e1
     -> (match reduce (e2,s) with Some (e2',s') -> Some (App(e1,e2',loc),s')
      | None -> None)
  | (App (e1, e2, loc), s) 
     -> (match reduce (e1,s) with Some (e1',s') -> Some (App(e1',e2,loc),s')
      | None -> None)

(*  Normal Order
  | (App (Fun(x,_,e1,_),e2,_),s) -> Some (subst x e2 e1, s)
  | (App (App(Z loc, g, loc1), v, loc2), s)
    -> Some (App (App (g, App(Z loc, g, loc),loc1), v, loc2), s)
  | (App (e1, e2, loc), s) ->
    (match reduce (e1,s) with Some (e1',s') -> Some (App(e1',e2,loc),s')
      | None -> (match reduce (e2,s) with Some (e2',s') -> Some (App(e1,e2',loc),s')
      | None -> None))
   | (Fun (x,t,e,loc),s) ->
    (match reduce (e,s) with Some (e',s') -> Some (Fun (x,t,e',loc),s') 
      | None -> None)
*)
  | _ -> None                                                    (*default*)


let string_of_config (p,m) = "<" ^ string_of_expr p ^ ", {" ^ string_of_mem m ^ "} >"

(* evaluate basically computes the transitive closure ->* of the
   one step reduction relation. *)
let rec evaluate debug c = match (reduce c) with
  | Some c' -> if debug 
               then Printf.printf "%s\n" (string_of_config c) 
               else () ; 
               evaluate debug c'
  | None -> c

