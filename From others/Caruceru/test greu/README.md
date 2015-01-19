# Problema 1 #

Implementati regulile sintactice, reprezentarea abstracta, 
functiile de traversare a sintaxei abstracte, regulile de tipuri si semantice 
pentru a adauga instructiunile ```print_int``` si ```read_int```.

## Syntaxa ##

expr ::= ... | print_int | read_int

## Tipuri ##

Gamma |= print_int : int -> unit

Gamma |= read_int : unit -> int

## Semantica ##

v ::= ... | print_int | read_int

< print_int n, s > -> < (), s >    efect lateral: n este afisat la consola

< read_int (), s > -> < n, s >     daca n este un intreg citit de la consola

Indicatie: folositi functiile OCaml print_int si read_int pentru
lucrul cu consola


# Problema 2 #

Implementati regulile de tipuri si regulile semantice pentru instructiunea
de atribuire conditionata data de sintaxa urmatoare:

## Sintaxa ##

expr ::= expr := expr when expr

Care este reprezentata in sintaxa abstracta folosind constructorul AtribWhen


## Tipuri ##

  Gamma |= e1 : ref T   Gamma |= e2 : T  Gamma |= e3 : bool
  ---------------------------------------------------------
            Gamma |= e1 := e2 when e3 : unit

## Semantica ##

                 < e1, s > --> < e1', s' >
   -----------------------------------------------------
   < e1 := e2 when e3, s > --> < e1' := e2 when e3, s' >

                < e2, s > --> < e2', s' >
   ---------------------------------------------------
   < l := e2 when e3, s > --> < l := e2' when e3, s' >

               < e3, s > --> < e3', s' >
   -------------------------------------------------
   < l := v when e3, s > --> < l := v when e3', s' >


   < l := v when true, s > --> < l := v, s >

   < l := v when false, s > --> < (), s >

Observati ca mai intai se evalueaza toate expresiile, de la stanga la dreapta,
propagandu-se astfel efectele laterale, si doar cand toate sunt evaluate se
ia decizia daca sa se efectueze sau nu atribuirea.

