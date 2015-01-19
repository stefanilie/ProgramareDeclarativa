open Mem
open ImpAST

(* types of locations *)
type tipL = TIntRef | TFloatRef | Type of tip

exception TypeError of expr*tip*tip
exception LocError of string*locatie
exception VarError of string*locatie

(* Type inference function *)
let rec infertype m = function
  | Int (n,_) -> TInt
  | Bool (b,_) -> TBool
  | Float (f,_) -> TFloat
  | Op(e1,Plus,e2,_) 
  | Op(e1,Minus,e2,_) 
  | Op(e1,Mul,e2,_) 
  | Op(e1,Div,e2,_) 
    -> (match (infertype m e1, infertype m e2) with
     | (TInt, TInt) -> TInt
     | (TFloat, TFloat) -> TFloat
     | (TInt, t) -> raise (TypeError (e2, TInt, t))
     | (TFloat, t) -> raise (TypeError (e2, TFloat, t))
     | (t,_) -> raise (TypeError (e1, TInt, t)))
  | Op(e1,Mic,e2,_) 
  | Op(e1,MicS,e2,_) 
    -> (match (infertype m e1, infertype m e2) with
     | (TInt, TInt) | (TFloat, TFloat)  -> TBool
     | (TInt, t)  -> raise (TypeError (e2, TInt, t))
     | (TFloat, t) -> raise (TypeError (e2, TFloat, t))
     | (t,_) -> raise (TypeError (e1, TInt, t)))
  | If(e1,e2,e3,_) -> (match (infertype m e1, infertype m e2, infertype m e3) with
     | (TBool, t, t') when t=t' -> t
     | (TBool, t, t') -> raise (TypeError (e3, t, t'))
     | (t,_,_) -> raise (TypeError (e1, TBool, t)))
  | Loc (l,loc) -> (try 
           (match lookup l m with 
              | TIntRef -> TInt
              | TFloatRef -> TFloat
              | _ -> failwith ("Expected " ^ l ^ " to be a location, not a variable")
           ) 
     with Not_found -> raise (LocError (l, loc)))
   | Var (v,loc) -> (try 
           (match lookup v m with 
              | Type t -> t
              | _ -> failwith ("Expected " ^ v ^ " to be a variable, not a location")
           ) 
     with Not_found -> raise (VarError (v, loc)))
  | Atrib(l,e,loc) -> (try (match (lookup l m, infertype m e) with
       | (TIntRef, TInt) | (TFloatRef, TFloat) -> TUnit
       | (TIntRef, t) -> raise (TypeError (e, TInt, t))
       | (TFloatRef, t) -> raise (TypeError (e, TFloat, t))
       | _ -> failwith ("Expected " ^ l ^ " to be a location, not a variable")
     ) with Not_found -> raise (LocError (l, loc)))
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
  | App (e1, e2, _) -> (match (infertype m e1, infertype m e2) with
     | (TArrow(t1, t1'), t2) when t1 = t2 -> t1'
     | (TArrow(t1, t1'), t2) -> raise (TypeError (e2,t1,t2))
     | (t1,t2) -> raise (TypeError (e1,TArrow(t2,t2),t1)))
  | IntOfFloat _ -> TArrow(TFloat, TInt)
  | FloatOfInt _ -> TArrow(TInt, TFloat)
  | Fun (x,t,e,_) -> TArrow(t, infertype (update_or_add (x,Type t) m) e)
  | Let (x,t,e1,e2,_) -> TArrow(TArrow(t, infertype (update_or_add(x,Type t) m) e1), infertype (update_or_add(x,Type t) m) e2)


let type_check m e = try
     let _ = infertype m e in true
  with 
    | TypeError (e,t1,t2) -> Printf.eprintf "%s\nError: Error: This expression has type %s but an expression was expected of type %s\n"  (location e) (string_of_tip t2) (string_of_tip t1) ; false
    | LocError (l,loc) -> Printf.eprintf "Error: Location %s undefined at %s.\n" l loc ; false
    | VarError (v,loc) -> Printf.eprintf "Error: Unbound variable %s at %s.\n" v loc ; false


