

# Problema #

Implementati regulile sintactice, reprezentarea abstracta,
functiile de traversare a sintaxei abstracte, regulile de tipuri si semantice
pentru adaugarea tipului de date inregistrare definit dupa cum
urmeaza:

## Sintaxa ##

    expr ::=  { eticheta = expr; eticheta = expr; }
              | expr . eticheta 

 
## Tipuri ##

                 Gamma |= e1 : t1   Gamma |= e2 : t2
            --------------------------------------------
		Gamma |= {e1 = exp1; e2 = exp2; } : {e1 : t1; e2 : t2;}


## Semantica ##


     O inregistrare care contine valori este o valoare
			  
      v :: = | {e1 = v; e2 = v;}
    

    Se evalueaza cele doua expresii dintr-o inregistrare pana la valori:


          < exp1, s > --> < exp1', s' >
    -------------------------------------
     <  {e1 = exp1; e2 = exp2;}, s > --> <  {e1 = exp1'; e2 = exp2;}, s' >
	 

		 < exp2, s > --> < exp2', s' >
	  -------------------------------------
	<  {e1 = v1; e2 = exp2;}, s > --> <  {e1 = v1; e2 = exp2';}, s' >	 
	 
   Operatorul punct "." isi evalueaza expresia 

			< e , s > --> < e', s' >
	-------------------------------------
	<  e . e1 , s > --> < e' . e1 , s' >
   
   si intoarce valoarea corespunzatoare unei etichete dintr-o inregistrare
   
   < {e1 = v1; e2 = v2;} . e1, s > --> 	< v1, s >
  

	< {e1 = v1; e2 = v2;} . e2, s > -->  < v2, s >
	
	Atentie!!!  - La implementare, pentru etichetele din inregistrare se vor folosi variabile