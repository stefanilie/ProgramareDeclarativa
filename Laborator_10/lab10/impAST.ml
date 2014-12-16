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
  | IntOfFloat of locatie | FloatOfInt of locatie | Z of locatie
  | Bool of bool * locatie
  | Int of int * locatie
  | Float of float * locatie
  | Loc of string * locatie
  | Var of string * locatie
  | DeBruijnVar of int * locatie
  | Op of expr * op * expr * locatie
  | Atrib of string * expr * locatie
  | Secv of expr * expr * locatie
  | If of expr * expr * expr * locatie
  | While of expr * expr * locatie
  | For of expr * expr * expr * expr * locatie | Skip of locatie
  | Fun of string * tip * expr * locatie
  | DeBruijnFun of expr * locatie
  | App of expr * expr * locatie

let exps = function
 | IntOfFloat _ | FloatOfInt _ | Bool _ | Int _ | Float _ 
 | Loc _ | Var _ |DeBruijnVar _ | Skip _ | Z _
   -> []
 | Atrib(_,e,_) | Fun(_,_,e,_) | DeBruijnFun (e,_)
   -> [e]
 | Op(e1,_,e2,_) | Secv(e1,e2,_) | While(e1,e2,_) | App(e1,e2,_)
   -> [e1;e2]
 | If(e1,e2,e3,_)
   -> [e1;e2;e3]
 | For(e1,e2,e3,e4,_)
   -> [e1;e2;e3;e4]

let revExps = function
   | (e,[]) -> e
   | (Atrib(l,_,loc),[e]) -> Atrib(l,e,loc)
   | (Fun(x,t,_,loc),[e]) -> Fun(x,t,e,loc) 
   | (DeBruijnFun(_,loc),[e]) -> DeBruijnFun(e,loc) 
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

let rec remove x = function
  | [] -> []
  | h::t when h < x -> h::(remove x t) 
  | h::t when h = x -> t
  | l -> l

let union l1 l2 = let rec punion = function
  | ([],l) -> l
  | (l,[]) -> l
  | (h1::t1,h2::t2) when h1=h2 -> h1::punion (t1,t2)
  | (h1::t1,h2::t2) when h1<h2 -> h1::punion (t1,h2::t2)
  | (h1::t1,h2::t2) -> h2::punion (h1::t1,t2)
 in punion (l1,l2)

let var e = 
  let var_fold = function
     | (Var (x,_),_) -> [x]
     | (Fun (x,_,_,_),[vs]) -> remove x vs
     | (_,vs_list) -> List.fold_left union [] vs_list
  in postVisit var_fold e

let vars = List.fold_left (fun l1  -> fun (_,(_,l2)) -> union l1 l2) []

let rec free vlist = 
  let max = List.fold_left 
     (fun m -> fun x -> 
       if String.get x 0 <> 'x' 
       then m 
       else try let m' = int_of_string (String.sub x 1 (String.length x - 1))
            in max m m'
            with Failure _ -> m) 0 vlist 
  in "x" ^ (string_of_int (max+1))

let rec invars x = function
  | [] -> false
  | h::t when h < x -> invars x t 
  | h::t when h = x -> true
  | _ -> false

let increaseIndexes = List.map (fun (x,n) -> (x,n+1))

let deBruijnify =
  let rec deBruijnify sigma =
    transform 
    (function
       | Var (x,l) -> Done (DeBruijnVar (List.assoc x sigma, l))
       | Fun (x,_,e,l) -> Done (DeBruijnFun(deBruijnify ((x,0)::(increaseIndexes sigma)) e, l))
       | _ -> More)
   (fun x -> x)
  in deBruijnify []

let rec deBruijnSubst n e =
  transform 
  (function
     | DeBruijnVar (n', l) as v -> Done (if n = n' then e else v)
     | DeBruijnFun(e', l) -> Done (DeBruijnFun(deBruijnSubst (n+1) e e',l))
     | _ -> More)
 (fun x -> x)

let rec substitute sigma = 
  transform 
  (function 
     | Var (x,l) -> (try 
                      let (sx,_) = List.assoc x sigma in Done sx 
                    with Not_found -> Done (Var (x,l)))
     | Fun (x,t,e,l) 
      -> let vs = vars sigma in
           if invars x vs || List.mem_assoc x sigma 
           then let x' = free (union vs (var e)) in
                Done (Fun (x',t,(substitute ((x,(Var (x',"new"), [x']))::sigma) e),l))
           else More
     | _ -> More)
  (fun x -> x)


let subst x ex e = substitute [x,(ex, var ex)] e

let string_of_expr e = 
  let string_of_expr_fold = function
  | (Z _,_) -> "Z"
  | (IntOfFloat _,_) -> "int_of_float"
  | (FloatOfInt _,_) -> "float_of_int"
  | (Int (i,_),_) -> string_of_int i
  | (Float (f,_),_) -> string_of_float f
  | (Bool (b,_),_) -> string_of_bool b
  | (Loc (s,_),_) -> "!" ^ string_of_l s
  | (Var (s,_),_) -> s
  | (DeBruijnVar (n,_),_) -> "U" ^ string_of_int n
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
  | (DeBruijnFun (_,_),[s]) -> 
    "(fun -> " ^ s ^ ")"
  | (App _, [s1;s2]) -> 
    " (" ^ s1 ^ s2 ^ ")"
  | _ -> failwith "This should not happen"
  in postVisit string_of_expr_fold e

let location = function
  | Z l
  | IntOfFloat l
  | FloatOfInt l
  | Int (_,l)
  | Float (_,l)
  | Bool (_,l)
  | Loc (_,l)
  | Var (_,l)
  | DeBruijnVar (_,l)
  | Op (_, _, _,l)
  | Atrib (_,_,l)
  | If (_, _, _,l)
  | While (_, _,l)
  | For (_, _, _, _,l)
  | Secv (_,_,l)
  | Skip l 
  | App (_,_,l)
  | Fun (_,_,_,l)
  | DeBruijnFun (_,l)
  -> l

let locations e =
   let locations_fold = function
          | (Loc (l,_),_) -> [l]
          | (Atrib(l,_,_),[locs]) -> union [l] locs
          | (_,locs_list) -> List.fold_left union [] locs_list
    in postVisit locations_fold e

