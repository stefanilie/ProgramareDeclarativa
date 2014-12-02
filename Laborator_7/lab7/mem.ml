let rec lookup x = function
  | [] -> raise Not_found
  | (y,v)::_ when x=y -> v
  | _::s -> lookup x s

let rec update (x,n) = function
  | [] -> raise Not_found
  | (y,v)::s  when x=y -> ((x,n)::s)
  | h::t -> h::(update (x,n) t)

let string_of_mem = 
  let string_of_pair (x,v) = x ^ " |-> " ^ string_of_int v in
  let rec string_of_mem str = function
  | [] -> str
  | p::t -> string_of_mem (str ^ "; " ^ string_of_pair p) t
  in function [] -> "" | p::t -> string_of_mem (string_of_pair p) t
