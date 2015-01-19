open Mem
open ImpAST

(** Type exception. [TypeError(e,t1,t2)] says that the expected type for
    [e] was [t1], but the computed type is [t2]. *)
exception TypeError of expr*tip*tip
(** Should take as argument a variable and says that that variable is unbound *)
exception VarNotFound of expr

(** Type inference function. Implements the typing rules, but also adds cases
    for failures to better localize the error. 
    @param m the typing environment, mapping variables to types. *)
let rec infertype (m:(string*tip) list) : expr -> tip = function
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
  | Deref (e,_)
    -> (match (infertype m e) with
          | TRef t -> t
          | t -> raise (TypeError (e, TRef t, t)))
  | Ref (e,_) -> TRef (infertype m e) 

  | Var (v,loc) -> (try 
           lookup v m 
     with Not_found -> raise (VarNotFound (Var(v, loc))))
  | Atrib(e1,e2,loc) -> (match (infertype m e1, infertype m e2) with
       | (TRef t1, t2) when t1 = t2 -> TUnit
       | (TRef t, t') -> raise (TypeError (e2, t, t'))
       | (t, t') -> raise (TypeError (e1, TRef t', t)))
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
  | Fun (x,t,e,_) -> TArrow(t, infertype (update_or_add (x,t) m) e)
  | Let (x,e1,e2,_) 
    -> let t = infertype m e1 in infertype (update_or_add (x,t) m) e2
  | LetRec (x,t,e1,e2,_) 
    -> let infertype' = infertype (update_or_add (x,t) m)
       in (match (infertype' e1, infertype' e2) with
             | (t1,t2) when t1 = t -> t2
             | (t1,_) -> raise (TypeError (e1,t,t1)))
  | Fst(e,_) -> ( match (infertype m e) with 
                  TMul(t1,t2) -> t1) 
  | Snd(e,_) -> ( match (infertype m e) with 
                  TMul(t1,t2) -> t2) 
  | Pair(e1,e2,_) -> ( match (infertype m e1, infertype m e2) with
                        (t1,t2) -> TMul(t1,t2))			  
  | e -> failwith ("infertype: Don't know how to type the following expression:\n" ^ string_of_expr e)


(** Type checks a program using the [infertype] function in the empty type 
    environment.  If the program typechecks, returns true.  If not, prints
    and adequate message and returns false. *)
let type_check e = try
     let _ = infertype [] e in true
  with 
    | TypeError (e,t1,t2) -> Printf.eprintf "%s\nError: This expression has type %s but an expression was expected of type %s\n"  (string_of_locatie (location e)) (string_of_tip t2) (string_of_tip t1) ; false
    | VarNotFound e -> Printf.eprintf "%s\nError: Variable %s unbound.\n"  (string_of_locatie (location e)) (string_of_expr e) ; false


