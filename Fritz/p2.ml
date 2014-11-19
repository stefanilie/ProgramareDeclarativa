type log = Bool of bool | Not of log | And of log * log | Var of string

let rec eval e = match e with
	Var x -> failwith "Eroare. Variabile nepermise!"
	|Bool true -> true
	|Bool false -> false
	|Not a -> not (eval a)
	|And (a,b)-> eval a && eval b


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
				 	if (eval b= false) then Bool false
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

let egal e1 e2 = if(Not e1 = Not e2)then true else false;;

let rec incl e1 e2 =match e2 with
	Bool a->if(egal e1 e2) then true else false;
	|Var a->if(egal e1 e2) then true else false;
	|Not a ->if(egal e1 e2) then true else incl e1 a
	|And(a,b)->if(egal e1 e2) then true else incl e1 a || incl e1 b
;;

