(** Module containing type declarations for expressions and types, 
    as well as generic functions for manipulating expressions.  *)


(** Type for representing locations in a file.  *)
type locatie = string * int * int * int * int
(** A location is a tuple (fname, bline, bcolumn, eline, ecolumn), where
    fname is the name of the file, bline and bcolumn give the line and column 
    where the element begins, while eline and ecolumn give the line and column
    where the element ends *)


(** Generates the string corresponding to a location in the source program  *)
let string_of_locatie (f,bl,bc,el,ec) = 
  Printf.sprintf "%s:%d.%d-%d.%d" f bl bc el ec


(** Exception constructor for expected matching errors *)
exception MatchError of string * locatie

(** Type for representing binary operation symbols.  *)
type op =
  | Plus
  | Mic
  | Minus
  | Mul
  | Div
  | MicS
  | Pct (**Operatorul punct*)    (*ADAUGAT*)

(** Generates the string corresponding to a binary operation symbol *)
let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | MicS -> "<"
  | Mic -> "<="
  | Pct -> "."  (**Operatorul punct*) (*ADAUGAT*)

(** Generates the string corresponding to a memory location.  *)
let string_of_l s = "_loc" ^ string_of_int s ^ "_"


(** Types of expressions *)
type tip = TInt 
  | TFloat 
  | TBool 
  | TUnit 
  | TArrow of tip * tip  (** [TArrow] encodes types associated to functions.
                             More precisely, [ TArrow(t1,t2) ] 
                             represents type {i t1 -> t2}.  *)
  | TRef of tip (** [TRef t] represents the type associated to references to 
                    memory locations storing elements of type t *)
  | TRec of string * tip * string * tip  (*ADAUGAT*) (**Asta e de forma asta pt ca asa e tipul structului.*)

(** Generates the string corresponding to the given expression of type tip *)
let rec string_of_tip = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TArrow (t1,t2) -> "(" ^ string_of_tip t1 ^ " -> " ^ string_of_tip t2 ^ ")"
  | TRef t -> string_of_tip t ^ " ref"
  | TRec (x,t1,y,t2) -> "{" ^ x ^ ":" ^ string_of_tip t1 ^ ";" ^ y ^ ":" ^ string_of_tip t2 ^ ";" ^ "}"  (*Tipuri record*)   (*ADAUGAT*)


(** The abstract type for representing IMP expressions *)
type expr =                                    (** e ::= *)
  | Bool of bool * locatie                         (** | b *)
  | Int of int * locatie                           (** | n *)
  | Float of float * locatie                       (** | f *)
  | Op of expr * op * expr * locatie               (** | e op e *)
  | Secv of expr * expr * locatie                  (** | e ; e *)
  | Skip of locatie                                (** | ()    *)
  | If of expr * expr * expr * locatie             (** | if e then e else e *)
  | While of expr * expr * locatie                 (** | while e do e done *)
  | For of expr * expr * expr * expr * locatie     (** | for(e;e;e)e *)
  | Var of string * locatie                        (** | x *)
  | Fun of string * tip * expr * locatie           (** | fun(x:T)->e *)
  | App of expr * expr * locatie                   (** | e e *)
  | IntOfFloat of locatie                          (** | int_of_float *)
  | FloatOfInt of locatie                          (** | float_of_int *)
  | Let of string * expr * expr * locatie          (** | let x = e in e *)
  | LetRec of string * tip * expr * expr * locatie (** | let rec x:T = e in e *)
  | Ref of expr * locatie                          (** | ref e *)
  | Deref of expr * locatie                        (** | ! e *)
  | Atrib of expr * expr * locatie                 (** | e := e *)
  | Loc of int * locatie                           (** | l  {i as a value} *)
  | Rec of string * expr * string * expr * locatie (** | {x = e1; y = e2;}*)  (*ADAUGAT*) (**Ce e mai sus dar cu locatie*)
  | FLFT of expr * expr * expr * locatie (** | f(e1, e2, e3)*)  (*ADAUGAT*)

(** Retrieves the file location component associated to the given expression *)
let location = function
  | Int (_,l) | Float (_,l) | Bool (_,l)
  | Loc (_,l) | Ref (_,l) | Deref (_,l) | Atrib (_,_,l) 
  | Op (_, _, _,l)
  | If (_, _, _,l) | While (_, _,l) | For (_, _, _, _,l)
  | Secv (_,_,l) | Skip l 
  | Var (_,l) | App (_,_,l) | Fun (_,_,_,l) | IntOfFloat l | FloatOfInt l
  | Let (_,_,_,l) | LetRec (_,_,_,_,l) | Rec(_,_,_,_,l) | Flft(_,_,_,_,l)  (*ADAUGAT*)  (** sunt 4 spatii pt ca un tip o variabila, un tip o variabila*)
  -> l

(** Returns the list of (direct) subexpressions of a given expression *)
let exps : expr -> expr list  = function
 | IntOfFloat _ | FloatOfInt _ | Bool _ | Int _ | Float _ | Loc _
 | Var _ | Skip _ 
   -> []
 | Ref (e,_) | Deref (e,_) | Fun(_,_,e,_) 
   -> [e]
 | Atrib(e1,e2,_) | Op(e1,_,e2,_) | Secv(e1,e2,_) | While(e1,e2,_) 
 | App(e1,e2,_) | Let (_,e1,e2,_) | LetRec (_,_,e1,e2,_) | Rec(_,e1,_,e2,_)   (*ADAUGAT*) (**  returneaza doar expresia, iar ce e dupa e2 e locatia. Fii atent cand 
caomletezi aici pt ca trebuie sa pui in fct de cate valori va returna, in acest caz 2*)
   -> [e1;e2]
 | If(e1,e2,e3,_) | Flft(e1,e2,e3,_)
   -> [e1;e2;e3]
 | For(e1,e2,e3,e4,_)
   -> [e1;e2;e3;e4]
(** For example, [exps (Op(Int (3,l),Plus Var ("x",l))) = [Int(3,l);Var("x",l)]]
*)

(** The reverse of the function above. Takes as argument a pair (e,l) where 
    e is an expression and l is list of expressions which must match the 
    direct subexpressions of e.  If that is the case, the direct subexpressions
    of e are replaced with the oned in l, from left to right. *)
let revExps : expr * expr list -> expr = function
   | (e,[]) -> e
   | (Deref(_,loc),[e]) -> Deref(e,loc)
   | (Ref(_,loc),[e]) -> Ref(e,loc)
   | (Fun(x,t,_,loc),[e]) -> Fun(x,t,e,loc) 
   | (Atrib(_,_,loc),[e1;e2]) -> Atrib(e1,e2,loc)
   | (Op(_,op,_,loc),[e1;e2]) -> Op(e1,op,e2,loc) 
   | (Secv(_,_,loc),[e1;e2]) -> Secv(e1,e2,loc) 
   | (While(_,_,loc),[e1;e2]) -> While(e1,e2,loc) 
   | (App(_,_,loc),[e1;e2]) -> App(e1,e2,loc) 
   | (Let(x,_,_,loc),[e1;e2]) -> Let(x,e1,e2,loc) 
   | (LetRec(x,t,_,_,loc),[e1;e2]) -> LetRec(x,t,e1,e2,loc) 
   | (If(_,_,_,loc), [e1;e2;e3]) -> If(e1,e2,e3,loc)
   | (For(_,_,_,_,loc), [e1;e2;e3;e4]) -> For(e1,e2,e3,e4,loc)
   | (Rec(x,_,y,_,loc),[e1;e2]) -> Rec(x,e1,y,e2,loc)  (*ADAUGAT*) (**Substitutie. curs 7*)
   | (Flft(_,_,_,loc),[e1,e2,e3]) -> Flft(e1,e2,e3,loc) (*Adaugat*)
   | (e,_) ->  raise (MatchError ("ImpAST.revExps", location e))
(** For example, 
    [revExps (Op(Int (3,l),Plus Var ("x",l)),  [Int(3,l);Int(7,l)]) =  
              Op(Int (3,l),Plus Int (7,l))] *)

(** Parameterized type for the result of the [pre] argument of a visitor *)
type 't preResult = 
  | More (** says the current expression should continue to be visited *)
  | Done of 't  (** Directly provides a a result for the current expression *)
 
(** {{: "http://en.wikipedia.org/wiki/Visitor_pattern"} Visitor pattern} 
    offering a generic mechanism for collecting information or transforming 
    expressions which allows focusion only on the aggregation/transformation
    part. 
    @param exp the expression being visited
    @param pre function to be applied before visiting an expression
               if result of pre is [More], the expression will be visited
               if result of pre is [Done r], then visit will evaluate to r
    @param post function to be applied after visiting exp's subexpressions
                takes as argument the expression and the list of results for
                its subexpressions and uses them to compute the final result.
*)
let rec visit 
         (pre : expr -> 't preResult) 
         (post : expr * 't list -> 't) 
         (exp : expr)
       : 't = 
  match pre exp with
    | More -> post (exp, (List.map (visit pre post) (exps exp)))
    | Done result -> result

(** An instance of the [visit] function above in which the [pre]-visitor
    always returns [More], which means the expression is completely visited. *)
let postVisit
       (post : expr * 't list -> 't)
     : expr -> 't =
  visit (fun e -> More) post

(** An instance of the [visit] function above used for transforming expressions
    To simplify the [post] function, we first apply [revExps] to put back the
    transformed sub-expressions in the original one.  This way, the [post]
    method simply takes expressions to expressions. *)
let transform
       (pre : expr -> expr preResult)
       (post : expr -> expr)
     : expr -> expr =
  visit pre (fun p -> post (revExps p))

(** Removes an element [x] from a sorted list *)
let rec remove x = function
  | [] -> []
  | h::t when h < x -> h::(remove x t) 
  | h::t when h = x -> t
  | l -> l

(** Merges two sorted list, removing the duplicates *)
let union l1 l2 = let rec punion = function
  | ([],l) -> l
  | (l,[]) -> l
  | (h1::t1,h2::t2) when h1=h2 -> h1::punion (t1,t2)
  | (h1::t1,h2::t2) when h1<h2 -> h1::punion (t1,h2::t2)
  | (h1::t1,h2::t2) -> h2::punion (h1::t1,t2)
 in punion (l1,l2)

(** Uses postVisit to collect all free variables of an expression [e].
    The aggregation function, [var_fold] implements the 4 interesting 
    cases [Var], [Fun], [Let], and [LetRec], and simply calls union
    on the list of variables produced by subexpressions for all other cases *)
let var e = 
  let var_fold = function
     | (Var (x,_),_) -> [x]
     | (Fun (x,_,_,_),[vs]) -> remove x vs
     | (Let (x,_,_,_), [vs1;vs2]) -> union vs1 (remove x vs2)
     | (LetRec (x,_,_,_,_), [vs1;vs2]) -> remove x (union vs1 vs2)
     | (_,vs_list) -> List.fold_left union [] vs_list
  in postVisit var_fold e

(** Gets all free variables ocurring in the image of substitution 
    @param sigma  : (string * (exp * string list)) list
           see substitution below. *)
let varsImSigma = List.fold_left (fun l1  -> fun (_,(_,l2)) -> union l1 l2) []

(** Returns the name of a variable not ocurring [vlist].
    The name is of the form xN where [N] is a number, computed
    to be the least number for which xN is not in the list *)
let rec free vlist = 
  let max = List.fold_left 
     (fun m -> fun x -> 
       if String.get x 0 <> 'x' 
       then m 
       else try let m' = int_of_string (String.sub x 1 (String.length x - 1))
            in max m m'
            with Failure _ -> m) 0 vlist 
  in "x" ^ (string_of_int (max+1))

(** Checks wehther [x] can be found in the sorted list [invars]. *)
let rec invars x = function
  | [] -> false
  | h::t when h < x -> invars x t 
  | h::t when h = x -> true
  | _ -> false

(** Implementation for substitution. 
    Instantiates the [transform] function where the [post] function is 
    the identity.
    As for free variables, we only need to explicitly define 
    the interesting cases [Var], [Fun], [Let] and [LetRec];  
    the others are handled by the recursion. 

    @param sigma  the substitution given as a list of pairs where the
                  first component is the name of the variable being replaced
                  while the second component is also a pair made of the
                  replacement expression and the list of its free variables. *)
let rec substitute (sigma : (string * (expr * string list)) list) = 
  transform 
  (function 
     | Var (x,l) -> (try 
                      let (sx,_) = List.assoc x sigma in Done sx 
                    with Not_found -> Done (Var (x,l)))
     | Fun (x,t,e,l) 
      -> let vs = varsImSigma sigma in
           if invars x vs || List.mem_assoc x sigma 
           then let x' = free (union vs (var e)) in
                Done (Fun (x',t,(substitute ((x,(Var (x',l), [x']))::sigma) e),l))
           else More
     | Let (x,e1,e2,l)
       -> let vs = varsImSigma sigma in
           if invars x vs || List.mem_assoc x sigma
           then let x' = free (union vs (var e2)) in
                Done (Let (x',substitute sigma e1, (substitute ((x,(Var (x',l), [x']))::sigma) e2),l))
           else More
     | LetRec (x,t,e1,e2,l)
       -> let vs = varsImSigma sigma in
           if invars x vs || List.mem_assoc x sigma
           then let x' = free (union (union vs (var e1)) (var e2)) in
                let sigma' = ((x,(Var (x',l), [x']))::sigma) in
                Done (LetRec (x',t,substitute sigma' e1, substitute sigma' e2,l))
           else More
     | _ -> More)
  (fun x -> x)


(** Substitution as it is used in the semantics.
    @param x the name of the variable being substituted
    @param ex the expression replacing free variables with name [x]
    @param e the expression in which to substitute
*)
let subst (x:string) (ex:expr) (e:expr) : expr = 
  substitute [x,(ex, var ex)] e

(** Computes the string representation of an expression [e].  Uses
    [postVisit] which allows writing a function relying on the fact that 
    the string representations of subexpressions are already computed. *)
let string_of_expr e = 
  let string_of_expr_fold = function
  | (IntOfFloat _,_) -> "int_of_float"
  | (FloatOfInt _,_) -> "float_of_int"
  | (Int (i,_),_) -> string_of_int i
  | (Float (f,_),_) -> string_of_float f
  | (Bool (b,_),_) -> string_of_bool b
  | (Loc (l,_),_) -> string_of_l l
  | (Ref _,[s]) -> "ref (" ^ s ^ ")"
  | (Deref _,[s]) -> "! (" ^ s ^ ")"
  | (Var (s,_),_) -> s
  | (Op (_,b, _,_),[s1;s2]) -> "(" ^ s1 ^ (string_of_op b) ^ s2 ^ ")"
  | (Atrib _,[s1;s2]) -> "(" ^ s1 ^ ":=" ^ s2 ^ ")"
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
  | (Let (x,_,_,_),[s1;s2]) -> 
    "(let " ^ x ^ " = " ^ s1 ^ " in " ^ s2 ^ ")"
  | (LetRec (x,t,_,_,_),[s1;s2]) -> 
    "(let rec " ^ x ^ ":" ^ string_of_tip t ^ " = " ^ s1 ^ " in " ^ s2 ^ ")"
  | (App _, [s1;s2]) -> 
    " (" ^ s1 ^ s2 ^ ")"
  | (Rec (x,_,y,_,_),[s1,s2]) -> "{" ^ x ^ "=" ^ s1 ^ ";" ^ y ^ "=" ^ s2 ^ ";" ^ "}"   (**Sintaxa record*)  (*ADAUGAT*) (**Cum arata sintaxa structului*)
  | (Flft (_,_,_,_),[s1,s2,s3]) -> "foldleft(" ^  s1 ^ ",	" ^ s2 ^ ", " ^ s3^ ")"    (*ADAUGAT*)
  | _ ->  let (f,l,c,_,_) as loc = location e in 
             raise (MatchError ("ImpAST.string_of_expr", loc))
  in postVisit string_of_expr_fold e

