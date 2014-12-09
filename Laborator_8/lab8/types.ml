open Mem
open ImpAST

(* types of expressions *)
type tip = TInt | TBool | TUnit

let string_of_tip = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"

(* types of locations *)
type tipL = TIntRef

exception TypeError of expr*tip*tip
exception LocError of string*locatie

(* Type inference function *)
let rec infertype m = function
  | Int (n,_) -> TInt
  | Bool (b,_) -> TBool
  | Op(e1,Plus,e2,_) 
  | Op(e1,Minus,e2,_) 
  | Op(e1,Mul,e2,_) 
  | Op(e1,Div,e2,_) 
    -> (match (infertype m e1, infertype m e2) with
     | (TInt, TInt) -> TInt
     | (TInt, t) -> raise (TypeError (e2, TInt, t))
     | (t,_) -> raise (TypeError (e1, TInt, t)))
  | Op(e1,Mic,e2,_) 
  | Op(e1,Egal,e2,_) 
    -> (match (infertype m e1, infertype m e2) with
     | (TInt, TInt) -> TBool
     | (TInt, t) -> raise (TypeError (e2, TInt, t))
     | (t,_) -> raise (TypeError (e1, TInt, t)))
  | If(e1,e2,e3,_) -> (match (infertype m e1, infertype m e2, infertype m e3) with
     | (TBool, t, t') when t=t' -> t
     | (TBool, t, t') -> raise (TypeError (e3, t, t'))
     | (t,_,_) -> raise (TypeError (e1, TBool, t)))
  | Loc (l,loc) -> (try (match lookup l m with TIntRef -> TInt) with 
     Not_found -> raise (LocError (l, loc)))
  | Atrib(l,e,loc) -> (try (match (lookup l m, infertype m e) with
     | (TIntRef, TInt) -> TUnit
     | (TIntRef, t) -> raise (TypeError (e, TInt, t))) with
     Not_found -> raise (LocError (l, loc)))
  | Skip _ -> TUnit
  | Secv (e1,e2,_) -> (match (infertype m e1, infertype m e2) with
     | (TUnit,t) -> t
     | (t1,_) -> raise (TypeError (e1, TUnit, t1)))
  | While (cond,body,_) -> (match (infertype m cond, infertype m body) with
     | (TBool, TUnit) -> TUnit
     | (TBool, t) -> raise (TypeError (body, TUnit, t))
     | (t,_) -> raise (TypeError (cond, TBool, t)))
  | For (init,cond,incr,body,_) 
    -> (match (infertype m init, infertype m cond, infertype m incr, infertype m body) with
     | (TUnit, TBool, TUnit, TUnit) -> TUnit
     | (TUnit, TBool, TUnit, t) -> raise (TypeError (body, TUnit, t))
     | (TUnit, TBool, t, _) -> raise (TypeError (incr, TUnit, t))
     | (TUnit, t, _, _) -> raise (TypeError (cond, TBool, t))
     | (t, _, _, _) -> raise (TypeError (init, TUnit, t)))


let type_check m e = try
     let _ = infertype m e in true
  with 
    | TypeError (e,t1,t2) -> Printf.eprintf "Error: Expected type %s but got type %s at %s.\n"  (string_of_tip t1) (string_of_tip t2) (location e) ; false
    | LocError (l,loc) -> Printf.eprintf "Error: Location %s undefined at %s.\n" l loc ; false


