type log = Bool of bool | Not of log | And of log * log | Var of string

let e1 = Not( And(Not (Bool true), Not (Bool false) ) )

let e2 = And(Not(And(Not (Bool true), Not (Var "x"))), Var "y")

let e3 = And(Not(And (Not(Var "y"), Not (Var "x"))), Var "y")

(* Definirea listei*)
let rec exists x y = match y with
 [] ->false
 |y::xs-> if(x=y) then true else exists x xs;;

let tail x =match x with 
 []->failwith "Eroare, lista vida."
 |x::xs -> xs;;

let head x= match x with
 []->failwith "Eroare, lista vida."
 |x::a -> x 
;;

 let append x y = 
 let x = x @ y in 
 x;;

let rec parcurgere e = match e with
 Var a -> [a]
 |Bool a->[""]
 |Not a -> append [] (parcurgere a)
 |And(a,b)->append [] (append (parcurgere a) (parcurgere b))

let rec find a =function 
 []->false
 |x::xs ->if (a=x)then true else find a xs;;

let rec exists a = function
 [] ->0
 |x::xs->if(a=x) then 1+ (exists a xs) else (0+exists a xs);;

let rec search_elemet =function
 [] -> true
 |x::xs -> if( find x xs) then 
 (if((exists x xs)+1 > 1) then false
 else search_elemet xs
 )
 else search_elemet xs

let rec get_element e = match e with
 Var x->true
 |Not a -> get_element a
 |And (a,b) -> get_element a || get_element b
 |Bool _ -> false;;

(* Functia rezolvare*)
let var_distincte e =
 if (get_element e = false) then true
else (
 let l = parcurgere e in
 search_elemet l);;
