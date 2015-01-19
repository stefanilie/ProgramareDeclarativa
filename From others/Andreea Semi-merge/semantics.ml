open Mem
open ImpAST

(** Returns true when the expression is a function value *)
let is_fun : expr -> bool = function
  | Fun _ | IntOfFloat _ | FloatOfInt _ -> true
  | _ -> false

(** Returns true when the expression is a value *)
let is_val : expr -> bool = function
  | Bool _ | Int _ | Float _ | Loc _ | Skip _ -> true
  | e -> is_fun e


(** Implements the reduction rules specifying the operational semantics. *)
let rec reduce = function
  | (Op(Int (n1,_),Plus,Int (n2,_),loc),s) -> Some (Int (n1+n2,loc),s)
  | (Op(Float (f1,_),Plus,Float (f2,_),loc),s) -> Some (Float (f1+.f2,loc),s)

  | (Op(Int (n1,_),Minus,Int (n2,_),loc),s) -> Some (Int (n1-n2,loc),s)
  | (Op(Float (f1,_),Minus,Float (f2,_),loc),s) -> Some (Float (f1-.f2,loc),s)

  | (Op(Int (n1,_),Mul,Int (n2,_),loc),s) -> Some (Int (n1*n2,loc),s)
  | (Op(Float (f1,_),Mul,Float (f2,_),loc),s) -> Some (Float (f1*.f2,loc),s)

  | (Op(Int (n1,_),Div,Int (n2,_),loc),s) when n2 <> 0 -> Some (Int (n1/n2,loc),s)
  | (Op(Float (f1,_),Div,Float (f2,_),loc),s) -> Some (Float (f1/.f2,loc),s)

  | (Op(Int (n1,_),Mic,Int (n2,_),loc),s) -> Some (Bool (n1<=n2,loc),s)
  | (Op(Float (f1,_),Mic,Float (f2,_),loc),s) -> Some (Bool (f1<=f2,loc),s)

  | (Op(Int (n1,_),MicS,Int (n2,_),loc),s) -> Some (Bool (n1<n2,loc),s)
  | (Op(Float (f1,_),MicS,Float (f2,_),loc),s) -> Some (Bool (f1<f2,loc),s)

  | (Op(Int (n1,loc1),op,e2,loc),s) ->                           (*OpDInt*)
    (match reduce (e2,s) with 
      | Some (e2',s') -> Some (Op(Int (n1,loc1),op,e2',loc),s')
      | None -> None
    )
  | (Op(Float (f1,loc1),op,e2,loc),s) ->                        (*OpDFloat*)
    (match reduce (e2,s) with 
      | Some (e2',s') -> Some (Op(Float (f1,loc1),op,e2',loc),s')
      | None -> None
    )
  | (Op(e1,op,e2,loc),s) ->                                     (*OpS*)
    (match reduce (e1,s) with Some (e1',s') -> Some (Op(e1',op,e2,loc),s')
      | None -> None)
  | (Op(e,Pct,e1,loc),s)                                       (*Evaluam expresia operatorului punct*)  (*ADAUGAT*)
     -> (match reduce (e,s) with 
	     |Some(e',s') -> Some (Op(e',Pct,e1,loc),s')
	     |None -> None)
  | (Op(Rec (x,e1,y,e2,_),Pct,x,loc),s) -> Some (e1,s)         (*Returnam valoarea etichetei x*)   (*ADAUGAT*)
  | (Op(Rec (x,e1,y,e2,_),Pct,y,loc),s) -> Some (e2,s)         (*Returnam valoarea etichetei y*)   (*ADAUGAT*)
  | (Deref (Loc (l,_), loc), s) -> Some (lookup l s, s)         (*Deref*)
  | (Deref (e, loc), s) ->                                      (*DerefS*)
    (match reduce (e,s) with Some (e',s') -> Some (Deref(e',loc),s')
      | None -> None)

  | (Ref (v,loc), s) when is_val v ->                           (*Ref*)
    let (l,s') = mem_add v s 
    in Some (Loc (l,loc), s')                    
  | (Ref (e, loc), s) ->                                        (*RefS*)
    (match reduce (e,s) with Some (e',s') -> Some (Ref(e',loc),s')
      | None -> None)
  | (Atrib(Loc(l,_), v,loc),s) when is_val v ->                 (*Atrib*)
      Some (Skip loc, update (l, v) s)
  | (Atrib(Loc(l,loc'),e,loc),s) ->                             (*AtribD*)
    (match reduce (e,s) with 
      | Some (e',s') -> Some (Atrib(Loc(l,loc'),e',loc),s')
      | None -> None)
  | (Atrib(e1,e2,loc),s) ->                                     (*AtribS*)
    (match reduce (e1,s) with 
      | Some (e1',s') -> Some (Atrib(e1',e2,loc),s')
      | None -> None)
  | (Secv(Skip _,e,_),s) -> Some (e,s)                          (*Secv*)
  | (Secv(e1,e2,loc),s) ->                                      (*SecvS*)
    (match reduce (e1,s) with Some (e1',s') -> Some (Secv(e1',e2,loc),s')
      | None -> None)
  | (If(Bool (true,_),e1,e2,_),s) -> Some (e1,s)                (*IfTrue*)
  | (If(Bool (false,_),e1,e2,_),s) -> Some (e2,s)               (*IfFalse*)
  | (If(e,e1,e2,loc),s) ->                                      (*IfS*)
    (match reduce (e,s) with Some (e',s') -> Some (If(e',e1,e2,loc),s')
      | None -> None)
  | (While(e1,e2,loc),s) -> 
     Some (If(e1,Secv(e2,While(e1,e2,loc),loc),Skip loc,loc),s) (*While*)
  | (For(init,cond,incr,body,l), s) 
    -> Some (Secv(init,While(cond,Secv(body,incr,l),l),l), s)   (*For*)
  | (App (IntOfFloat _, Float (f,_), loc), s)                   (*IntOfFloat*)
    -> Some (Int (int_of_float f, loc), s)
  | (App (FloatOfInt _, Int (n,_), loc), s)                     (*FloatOfInt*)
    -> Some (Float (float_of_int n, loc), s)
  | ((App (Fun(x,_,e1,_),e2,_) | Let (x,e2,e1,_)),s) when is_val e2 
    -> Some (subst x e2 e1, s)                                  (*App&Let *)
  | (App (e1, e2, loc), s) when is_fun e1                       (*AppR*)
     -> (match reduce (e2,s) with Some (e2',s') -> Some (App(e1,e2',loc),s')
      | None -> None)
  | (App (e1, e2, loc), s)                                      (*AppS*)
     -> (match reduce (e1,s) with Some (e1',s') -> Some (App(e1',e2,loc),s')
      | None -> None)
  | (Let (x, e2, e1, loc), s)                                   (*LetS*)
     -> (match reduce (e2,s) with 
           |Some (e2',s') -> Some (Let (x,e2',e1,loc),s')
           | None -> None)
  | (LetRec (x, t, e2, e1, loc), s)                             (*LetRec*)
     -> Some (subst x (LetRec (x, t, e2, e2, loc)) e1, s)
  | (Rec (x, e1, y, e2, loc), s)                               (*Evaluam e1 pana la o valoare atunci cand e2 este o expresie*)   (*ADAUGAT*)
     -> (match reduce (e1,s) with 
	     |Some (e1',s') -> Some (Rec (x,e1',y,e2,loc),s')
		 |None -> None)
  | (Rec (x, e1, y, e2, loc), s) when is_val e1                (*Evaluam e2 pana la o valoare atunci cand e1 este valoare*)    (*ADAUGAT*)
     -> (match reduce (e2,s) with 
	     |Some (e2',s') -> Some (Rec (x,e1,y,e2',loc),s')
		 |None -> None)
  | (Rec (x, e1, y, e2, loc), s) when is_val e2                (*e1 si e2 sunt valori*)  (*ADAUGAT*)
     -> Some (Rec (x,e1,y,e2,loc),s)
  |  (Foldleft(e1,e2,e3,loc),s)                                (*evaluarea primului argument*) (**here**)
      -> (match reduce(e1,s) with 
          |Some (e1',s') -> Some (Foldleft(e1',e2,e3,loc),s')
          |None -> None	)
  |	(Foldleft(e1,e2,e3,loc),s) when is_fun e1                        (**here**)
       -> (match reduce (e3,s) with
	      |Some (e3',s') -> Some (Foldleft(e1,e2,e3',loc),s')
		  |None -> None )
  | (Foldleft (e1,e2,e3,loc),s)                                      (**here**)
     ->( match reduce (e2,s) with 
	      |Some (f a b,e2,e3 )-> Some(f e2 Fst(e3),f e2 Snd(e3))
          |None->None) 
                                                                      (**PAIRS**)		  
	| (Pair(e1,e2,loc),s) when is_val e2                                         (*PairS*)
     -> (match reduce (e1,s) with
	     | Some (e1',s') -> Some (Pair(e1',e2,loc),s')
		 | None -> None)
  | (Pair(e1,e2,loc),s) when is_val e1                                            (*PairD*)
     -> (match reduce (e2,s) with
	     | Some (e2',s') -> Some (Pair(e1,e2',loc),s')
		 | None -> None)
    |(Fst(Pair(e1,e2,_),loc),s) when is_val e1                                     (*Fst*)
     -> Some (e1,s)
  |(Snd(Pair(e1,e2,_),loc),s) when is_val e2                                     (*Snd*)
     -> Some (e2,s) 
  | (Fst(e, loc), s)                                                            (*FstS*)
     -> (match reduce (e,s) with
	     | Some (e',s') -> Some (Fst(e',loc),s')
		 | None -> None)
  | (Snd(e, loc), s)                                                           (*SndS*)
     -> (match reduce (e,s) with
	     | Some (e',s') -> Some (Snd(e',loc),s')
		 | None -> None)
   | _ -> None                                                       (*default*)


(** Computes the string representation of a configuration *)
let string_of_config (p,m) = "<" ^ string_of_expr p ^ ", {" ^ string_of_mem m ^ "} >"

(** Computes the transitive closure ->* of the one step reduction relation. 
    Returns the final state reachable from the given (initial) configuration.
    @param debug Specifies whether each transition step should be printed
    @param c the configuration to be executed  *)
let rec evaluate debug c = match (reduce c) with
  | Some c' -> if debug 
               then Printf.printf "%s\n" (string_of_config c) 
               else () ; 
               evaluate debug c'
  | None -> c

