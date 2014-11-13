(* Types for the abstract syntax trees of IMP *)
type l = string
type op = Plus | Mic                  (* op ::= + | <=           *)
type e =                              (* e ::=                   *)
  | Int of int                        (*     n                   *)
  | Bool of bool                      (*   | b                   *)
  | Op of e * op * e                  (*   | e op e              *)
  | If of e * e * e                   (*   | if e then e else e  *)
  | Loc of l                          (*   | ! l                 *)
  | Atrib of l * e                    (*   | l := e              *)
  | Skip                              (*   | skip                *)
  | Secv of e * e                     (*   | e ; e               *)
  | While of e * e                    (*   | while e do e        *)

(* The state is a list of pairs (location,integer) *)
type state = (l * int) list

let rec lookup x = function
  | [] -> None
  | (y,v)::_ when x=y -> Some v
  | _::s -> lookup x s

let rec update (x,n) = function
  | [] -> None
  | (y,v)::s  when x=y -> Some ((x,n)::s)
  | h::t -> match update (x,n) t with None -> None | Some t' -> Some (h::t')

(* Configuration is described as an expression-state pair *)
type config = e * state

(* The reduce function defines the one step relation.  It is again partial
   as it has to account for final and stuck configurations  *)
let rec reduce = function
  | (Op(Int n1,Plus,Int n2),s) -> Some (Int (n1+n2),s)             (*Op+*)
  | (Op(Int n1,Mic,Int n2),s) -> Some (Bool (n1<=n2),s)            (*Op<=*)
  | (Op(Int n1,op,e2),s) ->                                        (*OpD*)
    (match reduce (e2,s) with
      | None -> None
      | Some (e2',s') -> Some (Op(Int n1,op,e2'),s')
    )
  | (Op(e1,op,e2),s) ->                                            (*OpS*)
    (match reduce (e1,s) with
      | None -> None
      | Some (e1',s') -> Some (Op(e1',op,e2),s')
    )
  | (Loc l, s) ->                                                  (*Loc*)
    (match lookup l s with
      | None -> None
      | Some n -> Some (Int n, s)
    )
  | (Atrib(l, Int n),s) ->                                         (*Atrib*)
    (match update (l,n) s with
      | None -> None
      (*Aici am schimbat ai atribuirea sa fie expresie de tip intreg*)
      (*2)**| Some s' -> Some (Int n, s')*)
      | Some s'-> match lookup l s with
        |None -> None
        |Some k -> Some(Int k, s')

    )
  | (Atrib(l,e),s) ->                                              (*AtribD*)
    (match reduce (e,s) with
      | None -> None
      | Some (e',s') -> Some (Atrib(l,e'),s')
    )
    (*Skip-> Int n*)
  | (Secv(Int n,e),s) -> Some (e,s)                                 (*Secv*)
  | (Secv(e1,e2),s) ->                                             (*SecvS*)
    (match reduce (e1,s) with
      | None -> None
      | Some (e1',s') -> Some (Secv(e1',e2),s')
    )
  | (If(Bool true,e1,e2),s) -> Some (e1,s)                         (*IfTrue*)
  | (If(Bool false,e1,e2),s) -> Some (e2,s)                        (*IfFalse*)
  | (If(e,e1,e2),s) ->                                             (*IfS*)
    (match reduce (e,s) with
      | None -> None
      | Some (e',s') -> Some (If(e',e1,e2),s')
    )
  | (While(e1,e2),s) -> Some (If(e1,Secv(e2,While(e1,e2)),Skip),s) (*While*)
  | _ -> None                                                      (*default*)


(* evaluate basically computes the transitive closure ->* of the
   one step reduction relation. *)
let rec evaluate c = match reduce c with
  | None -> c
  | Some c' -> evaluate c'


(****************************************************
        Types and Type Inference
*****************************************************)

(* types of expressions *)
type tip = TInt | TBool | TUnit

(* types of locations *)
type tipL = TIntRef

(* Type Environment *)
type mediuTip = (l*tipL) list

(* Type inference function *)
let rec infertype m = function
  | Int n -> Some TInt
  | Bool b -> Some TBool
  | Op(e1,Plus,e2) -> (match (infertype m e1, infertype m e2) with
     | (Some TInt, Some TInt) -> Some TInt
     | _ -> None)
  | Op(e1,Mic,e2) -> (match (infertype m e1, infertype m e2) with
     | (Some TInt, Some TInt) -> Some TBool
     | _ -> None)
  | If(e1,e2,e3) -> (match (infertype m e1, infertype m e2, infertype m e3) with
     | (Some TBool, Some t, Some t') when t=t' -> Some t
     | _ -> None)
  | Loc l -> (match lookup l m with
     | Some TIntRef -> Some TInt
     | _ -> None)
  | Atrib(l,e) -> (match (lookup l m, infertype m e) with
    (*Some TUnit ->some TInt*)
     | (Some TIntRef, Some TInt) -> Some TInt
     | _ -> None)
  | Skip -> Some TUnit
  | Secv (e1,e2) -> (match (infertype m e1) with
    (*TUnit -> TInt*)
     | Some TInt -> infertype m e2
     | _ -> None)
  | While (e1,e2) -> (match (infertype m e1, infertype m e2) with
     | (Some TBool, Some TUnit) -> Some TUnit
     | _ -> None)


(***********************************************************
   Test Program
************************************************************)

let test_pgm = Secv(Atrib("y", Int 0),
                    While(Op(Int 1,Mic, Loc "x"),
                          Secv(Atrib("y", Op(Loc "y", Plus, Loc "x")), 
                               Atrib("x", Op(Loc "x", Plus, Int (-1))))))

let test_config n = (test_pgm,[("x",n);("y", 0)])

let rec gamma_of_s = function
  | [] -> []
  | (l,v)::t -> (l,TIntRef)::gamma_of_s t

let type_and_run (p,s) = match infertype (gamma_of_s s) p with
  | None -> failwith "Eroare de tip!  Verificati programul"
  | Some t -> evaluate (p,s)

(***
1)  type_and_run(If(Int 1, Int 2, Bool true), []);;
***)

