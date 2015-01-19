let f1 x = x+1
let f2 x = x+2
let f3 =f1+f2;;

let p x = print_endline (string_of_int (x));;

let rec ex x=  
	if x=0 then x
else p (x); ex x-1)z

let rec fact x=
	if x=0 then 1
	else
	(
	x*fact(x-1)
	);;


let rec cmmdc a b =
	if a=b then a
else
	(
		if a>b then cmmdc (a-b) b
	else cmmdc a (b-a)
	)

let f1 x = x+1
let f2 x = x+2

let f3 f1 =	fun x-> (f1 x);;
let f3 f1 f2 x = (f1 x) + (f2 x);;

let compune f1 f2 x = f2 (f1 x );;

let rec fib = function
	 0 -> 0
	|1-> 1
	|n -> fib (n-1) + fib(n-2);;


let fibaux x =  

let rec fib2 = function
	0 -> 0
	| 1 ->1
	| n -> let rec fib n x i j =
	if(n=1) then x
else 
	(
		(fib (n-1) (i+j) j  (i+j))
	);;


let rec fib n x i j =
	if(n=1) then x
else 
	(
		(fib (n-1) (i+j) j  (i+j))
	);;

exception Vid of 'a list;;

let p x = if x=[] then raise (Vid x) else x;;


let prim2 y= 
	let rec p x= match x with
	[]->raise Vid	
	|x::[]-> x
	|x::a -> p a in
	
	try p y with Vid -> "Eroare. Lista este vida!";;


	let prim y= 
	let rec p x= match x with
	[]->raise Vid
	|x::a -> x in
	
	try p y with Vid -> "Eroare. Lista este vida!";;


let tail x =match x with 
	[]->raise Vid
	|x::xs -> xs;;


let prim3 y= 
	let rec p x= match x with
	[]->raise Vid	
	|x::[]-> raise Vid
	|x::xs::[]->x
	|x::xs -> p xs in
	
	try p y with Vid -> "Eroare. Lista este vida!";;


let rec len x= match x with
	[]->0
	|x::xs -> 1 + (len xs);;

let lip x y = 
	let x =	x @ y in 
	x;;

let rec lip2 x y =match x with
	[] -> y
	|x::xs->lip2 xs y;;




let rec inv = function
	[] -> []
	|x::xs -> inv(xs) @ (x::[])
;;


let rec ap x y =match y with
	[] ->false
	|y::xs-> if(x=y) then true else ap x xs;;

let rec afla x l =match l with (y,nr)::xs-> if(x=y) then nr else afla x xs;;

let nr y = y+1;

let pozitie x l =
	let  c = 


let rec p x l =match l with
	[] -> (List.length l)-5
	|y::xs ->if(y=x) then 1 else 1+ ( p x xs);;

let pozitie x l =
	let poz = p x l in
		if(poz <0)then -1 else poz;;

p 7 [1;2;3;4];;


let rec interclas a b = match a with
	[]->[]
	|x::xs -> if(x < (prim b)) 
					then ( (x::[]) @ (interclas (tail a) b))
				else interclas b (tail a) ;;

interclas ["ana"; "are"; "mere"] ["gina"; "hare"; "pere"];;	


let rec divide a c=match a with
	[]->[]
	|x::xs ->if(c=0) then ( x @ (divide xs 1), []) else ([], x @ (divide xs 0) );;


--------------------------------------- lab 2 + ----------------------------




type log = Bool of bool | Not of log | And of log * log | Var of string

let e1 = Not(And(Not (Bool true), Not (Bool false)))


let e1 = Not (Bool true)

let e2 = And(Not(And(Not (Bool true), Not (Var "x"))), Var "y")

1.  Să se scrie o funcție string_of_log care ia ca argument o expresie logică și întoarce un șir de caractere care reprezintă frumos expresia.  Exemple de rulare:

# string_of_log e1;;
- : string = "!(!true && !false)"

# string_of_log e2;;
- : string = "(!(!true && !x) && y)"


let rec string_of_log e = match e with
	Bool true ->"true"
	|Bool false ->"false"
	|Var a -> a
	|Not a -> "!" ^ (string_of_log a) 
	|And (a,b) ->  "(" ^ (string_of_log a) ^ " && " ^ (string_of_log b) ^ ")"
;;

--



# eval e1;;
- : bool = true

# eval e2;;
Exception: Failure "Expresia are variabile".


let rec eval e = match e with
	Var x -> failwith "Eroare. Variabile nepermise!"
	|Bool true -> true
	|Bool false -> false
	|Not a -> not (eval a)
	|And (a,b)-> eval a && eval b
;;

--
3. Să se scrie o funcție evalueaza care ia ca argument o listă de perechi 
nume-valoare de adevăr și o expresie logică cu variabile și întoarce valoarea 
ei de adevăr.  Exemple:

# evalueaza [] e1;;
- : bool = true

# evalueaza [("x",true);("y",false)] e2;;
- : bool = false

# evalueaza [("x",true);("y",true)] e2;;
- : bool = true

# evalueaza [("x",true);("y",true); ("z",true)] e2;;

let rec find a = function
	[]->failwith "Eroare. Valoarea nu a fost gasita"
	|(x,b)::xs -> if (a=x)then b else find a xs;;
	
let rec evalueaza l e = if l!=[] then match e with
	Var x -> find x l
	|Bool true -> true
	|Bool false -> false
	|Not a -> not (eval a)
	|And (a,b)-> evalueaza l a && evalueaza l b
else 
	eval e
;;

----
Să se scrie o funcție simplifica care reduce o expresie logică la forma cea mai simplă.
 Exemple:

# simplifica e1;;
- : log = Bool true

# simplifica e2;;
- : log = Var "y"

let rec findv e = match e with
	Var x->true
	|Not a -> findv a
	|And (a,b) -> findv a || findv b
	|Bool _ -> false;;

let rec finds e = function
	|Bool a -> Bool a					
	|Var a -> Var a
	|And (a,b) -> let eva= findv a in 
				  let evb= findv b in
				 if (eva && not evb )then
				 	if (evb = false) then Bool false
				 		else finds(a)
				 	else 
				 		(
				 			if (evb = true && eva)
				 				then And (finds a, finds b)
				 			else 
				 				finds(b)
				 		)
	|Not a -> let eva = findv a in 
				if (not eva) then Not(eval a)
					else Not (finds a)

;;

and (false _ ) -> false


let simplifica e = if (findv e = false) then Bool (eval e) 
else 



;;

: string = "(! ( !true && !x ) && y)"
--




--------------------------------alina---------------
1.  Sa se scrie o func?ie string_of_log care ia ca argument o expresie logica ?i întoarce un ?ir de caractere care reprezinta frumos expresia.

type log = Bool of bool | Not of log | And of log * log | Var of string;;
let e1 = Not(And(Not (Bool true), Not (Bool false)));;
let e2 = And(Not(And(Not (Bool true), Not (Var "x"))), Var "y");;
let rec string_of_log log  = match log  with
	| And (x,y) -> "(" ^ string_of_log x ^ " && " ^ string_of_log y ^ ")"
	| Not x -> "!" ^ string_of_log x
	| Var x -> x
	| Bool b -> string_of_bool b ;;
  
2. Sa se scrie o func?ie eval care ia ca argument o expresie logica ?i întoarce valoarea ei de adevar. Daca expresia con?ine variabile, va e?ua.

let rec eval log = match log with
	| And (x,y) -> (eval x) && (eval y)
	| Not x -> not (eval x)
	| Var x -> failwith "Expresia are variabile."
	| Bool b -> b 

3. Sa se scrie o func?ie evalueaza care ia ca argument o lista de perechi nume-valoare de adevar ?i o expresie logica cu variabile ?i întoarce valoarea ei de adevar.

let rec find a lista = match lista with
	| (x,y)::rest -> if a = x then y else find a rest 
	| [] -> failwith "ceva"
let rec evalueaza lista log = match log with
	| And (x,y) -> (evalueaza lista x) && (evalueaza lista y)
	| Not x -> not (evalueaza lista x)
	| Var x -> find x lista
	| Bool b -> b

4. Sa se scrie o func?ie simplifica care reduce o expresie logica la forma cea mai simpla.


let rec simplifica exp = match exp with
	| Var a -> Var a
	| Bool b -> Bool b
	| Not e -> (match e with 
					| Var a -> Not e
					| Bool b -> if b = true then (Bool false) else (Bool true)
					| And (e1,e2) -> simplifica (Not(simplifica (And ((simplifica e1),(simplifica e2)))))
					| Not x -> Not (simplifica x))
	| And (Bool true, e2) -> simplifica e2
	| And (Bool false, e2) -> Bool false
	| And (e1, Bool true) -> simplifica e1
	| And (e1, Bool false) -> Bool false
	| And (e1,e2) -> simplifica (And (simplifica e1, simplifica e2))
	;;
