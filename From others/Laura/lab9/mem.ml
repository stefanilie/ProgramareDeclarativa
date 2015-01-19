let lookup = List.assoc

let rec update (x,n) = function
  | [] -> raise Not_found
  | (y,v)::s  when x=y -> ((x,n)::s)
  | h::t -> h::(update (x,n) t)

let update_or_add (x,v) m = (x,v)::List.remove_assoc x m

let string_of_mem = 
  let string_of_pair (x,v) = x ^ " |-> " ^ ImpAST.string_of_expr v in
  let rec string_of_mem str = function
  | [] -> str
  | p::t -> string_of_mem (str ^ "; " ^ string_of_pair p) t
  in function [] -> "" | p::t -> string_of_mem (string_of_pair p) t
