open ImpAST

(** memory lookup is simply the [assoc] function defined in the List module *)
let lookup = List.assoc

(** Adds a new location containing value [n] to the given memory state and
    returns a pair of the noew location and the memory state.
    Assumes the first location in memory has the highest index 
    (was the last added). *)
let mem_add (n : 't) : (int*'t) list -> int * (int*'t) list = function
  | [] -> (0,[0,n])
  | (x',n')::t -> let x = x'+1 in (x,(x,n)::(x',n')::t)

(** Updates te value of [x] to [n] in the given memory state.
    @raise Not_found exception if [x] cannot be found. *)
let rec update (x,n) = function
  | [] -> raise Not_found
  | (y,v)::s  when x=y -> ((x,n)::s)
  | h::t -> h::(update (x,n) t)

(** Similar to the above, but instead of failing when [x] is not found, it
    adds a new entry for [x] in the memory *)
let update_or_add (x,v) m = (x,v)::List.remove_assoc x m

(** Creates a string representation for the given memory state *)
let string_of_mem : (int*expr) list -> string = 
  let string_of_pair (x,v) = string_of_int x ^ " |-> " ^ string_of_expr v in
  let rec string_of_mem str = function
  | [] -> str
  | p::t -> string_of_mem (str ^ "; " ^ string_of_pair p) t
  in function [] -> "" | p::t -> string_of_mem (string_of_pair p) t
