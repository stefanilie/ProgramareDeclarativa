## Sintaxa ##

expr ::= foldleft (expr,expr,expr)


## Semantica intuitiva ##

Foldleft primeste 3 argumente: o functie cu 2 argumente, o expresie si o pereche si are rezultat perechea calculata astfel

foldleft(f, a, (b,c)) = (f(a b), f(a c))

## Tipuri ##

Gamma |= e1 : t1 -> (t2 -> t3) Gamma |= e2 : t1 Gamma |= e3 : t2 * t1 
-----------------------------------------------------------
Gamma |= foldleft(e1,e2,e3) : t1


## Semantica ##



Operatorul foldleft isi evalueaza primul argument 

< e1, s > --> < e1', s' >
-------------------------------------
< foldleft (e1,e2,e3), s > --> < foldleft (e1',e2,e3), s' >

Cand primul argument este o functie, se trece la evaluarea argumentului 3 

< e3, s > --> < e3', s' >
------------------------------------- when is_fun e1
< foldleft (e1,e2,e3'), s > --> < foldleft (e1,e2,e3'), s' >

Dupa evaluarea celor 2 argumente, se poate construi perechea rezultat a lui foldleft 

< foldleft (f a b, e2,(v1,v2)), s > --> (f e2 v1, f e2 v2)

Atentie! 

1) Pentru apelarea unei functii (fun(x:int) -> x + 1) 5, in limbajul IMP folosim App(Fun(x,t,x+1,loc),5,loc') 
2) O functie binara apelata cu primul argument x = 2 astfel fun (x : int)-> fun(y : int)-> x + y) 2 este functia unara fun(y : int)-> 2 + y





ImpAST->types->semantics->lexer->parser
