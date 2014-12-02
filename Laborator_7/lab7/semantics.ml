open Mem
open ImpAST

let rec reduce = function
  | (Op(Int (n1,_),Plus,Int (n2,_),loc),s) -> Some (Int (n1+n2,loc),s)             (*Op+*)
  | (Op(Int (n1,_),Minus,Int (n2,_),loc),s) ->Some (Int (n1-n2,loc),s)             (*Op-*)
  | (Op(Int (n1,_),Mic,Int (n2,_),loc),s) -> Some (Bool (n1<=n2,loc),s)            (*Op<=*)
  | (Op(Int (n1,loc1),op,e2,loc),s) ->                                        (*OpD*)
    (match reduce (e2,s) with 
      | Some (e2',s') -> Some (Op(Int (n1,loc1),op,e2',loc),s')
      | None -> None
    )
  | (Op(e1,op,e2,loc),s) ->                                            (*OpS*)
    (match reduce (e1,s) with Some (e1',s') -> Some (Op(e1',op,e2,loc),s')
      | None -> None)
  | (Loc (l,loc), s) -> Some (Int (lookup l s,loc), s)                    (*Loc*)
  | (Atrib(l, Int (n,_),loc),s) ->                                         (*Atrib*)
      Some (Skip loc, update (l,n) s)
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
  | _ -> None                                                    (*default*)


(* evaluate basically computes the transitive closure ->* of the
   one step reduction relation. *)
let rec evaluate c = match (reduce c) with
  | Some c -> evaluate c
  | None -> c


let string_of_config (p,m) = "<" ^ string_of_expr p ^ ", {" ^ string_of_mem m ^ "} >"
