let inmultire a b =
	let x= ref a 
	and y= ref b 
	and z=ref 0 in 
		z:=0;
		while 1 <= !x do 
			z := !z + !y;
			x := !x + -1
		done 
; [("x",!x);("y",!y);("z",!z)];;


let test n =
  let x = ref n
  and y = ref 0
  in y := 0 ;
     while 1 <= !x do
       y := !y + !x;
       x := !x + -1
     done
     ; [("x",!x);("y",!y)];;


let test_produs =
	Secv(Atrib("z",Int 0),
		While(Op(Int 1,Mic, Loc "x"),
				Secv(
						Atrib("z", Op(Loc "z", Plus, Loc "y")),
						Atrib("x", Op(Loc "x", Plus, Int (-1)))
				)
			 )
		)


 let test_config a b = (test_produs,[("x",a);("y", b);("z",0)])


 evaluate (test_config 10 43);;

 -------------------------	
let [("x",x);("y",y);("z",`z)] = ref [("",0);("",0);("",0)];; 

let fact n 