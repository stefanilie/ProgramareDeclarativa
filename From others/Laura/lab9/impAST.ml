type locatie = string

type op =
  | Plus
  | Mic
  | Minus
  | Mul
  | Div
  | MicS

let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | MicS -> "<"
  | Mic -> "<="

let string_of_l s = s

(* types of expressions *)
type tip = TInt | TFloat | TBool | TUnit | TArrow of tip * tip
let rec string_of_tip = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TArrow (t1,t2) -> "(" ^ string_of_tip t1 ^ " -> " ^ string_of_tip t2 ^ ")"


type expr =
  | IntOfFloat of locatie | FloatOfInt of locatie 
  | Bool of bool * locatie
  | Int of int * locatie
  | Float of float * locatie
  | Loc of string * locatie
  | Var of string * locatie
  | Op of expr * op * expr * locatie
  | Atrib of string * expr * locatie
  | Secv of expr * expr * locatie
  | If of expr * expr * expr * locatie
  | While of expr * expr * locatie
  | For of expr * expr * expr * expr * locatie | Skip of locatie
  | Fun of string * tip * expr * locatie
  | App of expr * expr * locatie
  | Let of string * tip * expr * expr * locatie

let exps = function
 | IntOfFloat _ | FloatOfInt _ | Bool _ | Int _ | Float _ 
 | Loc _ | Var _  | Skip _ 
   -> []
 | Atrib(_,e,_) | Fun(_,_,e,_) 
   -> [e]
 | Op(e1,_,e2,_) | Secv(e1,e2,_) | While(e1,e2,_) | App(e1,e2,_) | Let(_,_,e1,e2,_)
   -> [e1;e2]
 | If(e1,e2,e3,_)
   -> [e1;e2;e3]
 | For(e1,e2,e3,e4,_)
   -> [e1;e2;e3;e4]

let revExps = function
   | (e,[]) -> e
   | (Atrib(l,_,loc),[e]) -> Atrib(l,e,loc)
   | (Fun(x,t,_,loc),[e]) -> Fun(x,t,e,loc) 
   | (Let(x,t,_,_,loc),[e1;e2]) -> Let(x,t,e1,e2,loc)
   | (Op(_,op,_,loc),[e1;e2]) -> Op(e1,op,e2,loc) 
   | (Secv(_,_,loc),[e1;e2]) -> Secv(e1,e2,loc) 
   | (While(_,_,loc),[e1;e2]) -> While(e1,e2,loc) 
   | (App(_,_,loc),[e1;e2]) -> App(e1,e2,loc) 
   | (If(_,_,_,loc), [e1;e2;e3]) -> If(e1,e2,e3,loc)
   | (For(_,_,_,_,loc), [e1;e2;e3;e4]) -> For(e1,e2,e3,e4,loc)
   | _ -> failwith "this should not happen"
 

type 't preResult = More | Done of 't
 
let rec visit pre post exp = 
  match pre exp with
    | More -> post (exp, (List.map (visit pre post) (exps exp)))
    | Done result -> result

let postVisit post = visit (fun e -> More) post

let transform pre post = visit pre (fun p -> post (revExps p))

let subst x ex e = e

let string_of_expr e = 
  let string_of_expr_fold = function
  | (IntOfFloat _,_) -> "int_of_float"
  | (FloatOfInt _,_) -> "float_of_int"
  | (Int (i,_),_) -> string_of_int i
  | (Float (f,_),_) -> string_of_float f
  | (Bool (b,_),_) -> string_of_bool b
  | (Loc (s,_),_) -> "!" ^ string_of_l s
  | (Var (s,_),_) -> s
  | (Op (_,b, _,_),[s1;s2]) -> "(" ^ s1 ^ (string_of_op b) ^ s2 ^ ")"
  | (Atrib (s,_,_),[s2]) -> "(" ^ string_of_l s ^ ":=" ^ s2 ^ ")"
  | (If _,[s1;s2;s3]) ->
    "(if " ^ s1 ^ "\nthen " ^ s2 ^ "\nelse " ^ s3 ^ ")"
  | (While _,[s1;s2]) ->
    "while " ^ s1 ^ " do \n" ^ s2 ^ "\ndone"
  | (For _,[s1;s2;s3;s4])
    -> "(for (" ^ s1 ^ "; " ^ s2 ^ "; " ^ s3 ^ ") \n" ^ s4 ^ "\n)"
  | (Secv _,[s1;s2]) ->
    "(" ^ s1 ^ ";\n" ^ s2 ^ ")"
  | (Skip _, _) -> "()"
  | (Fun (x,t,_,_),[s]) -> 
    "(fun (" ^ x ^ ":" ^ string_of_tip t ^ ") -> " ^ s ^ ")"
  | (App _, [s1;s2]) -> 
    " (" ^ s1 ^ s2 ^ ")"
  | (Let (x,t,_,_,_),[s1;s2]) -> "let " ^ x ^ ":" ^ string_of_tip t ^ "=" ^ s1 ^ "in" ^ s2
  | _ -> failwith "This should not happen"
  in postVisit string_of_expr_fold e

let location = function
  | IntOfFloat l
  | FloatOfInt l
  | Int (_,l)
  | Float (_,l)
  | Bool (_,l)
  | Loc (_,l)
  | Var (_,l)
  | Op (_, _, _,l)
  | Atrib (_,_,l)
  | If (_, _, _,l)
  | While (_, _,l)
  | For (_, _, _, _,l)
  | Secv (_,_,l)
  | Skip l 
  | App (_,_,l)
  | Fun (_,_,_,l)
  | Let (_,_,_,_,l)
  -> l

let union l1 l2 = let rec punion = function
  | ([],l) -> l
  | (l,[]) -> l
  | (h1::t1, h2::t2) when h1 = h2 -> h1 :: punion (t1,t2)
  | (h1::t1, h2::t2) when h1 < h2 -> h1 :: punion (t1,h2::t2)
  | (h1::t1, h2::t2) -> h2 :: punion (h1::t1,t2)
in punion (l1,l2)
  


let locations e =
   let locations_fold = function
          | (Loc (l,_),_) -> [l]
          | (Atrib(l,_,_),[locs]) -> union [l] locs
          | (_,locs_list) -> List.fold_left union [] locs_list
    in postVisit locations_fold e

