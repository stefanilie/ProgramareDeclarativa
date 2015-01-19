type locatie = string

type op =
  | Plus
  | Mic
  | Minus
  | Mul
  | Div
  | Egal

let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Egal -> "=="
  | Mic -> "<="
  
type expr =
  | Bool of bool * locatie
  | Int of int * locatie
  | Float of float * locatie
  | Loc of string * locatie
  | Op of expr * op * expr * locatie
  | Atrib of string * expr * locatie
  | Secv of expr * expr * locatie
  | If of expr * expr * expr * locatie
  | While of expr * expr * locatie
  | For of expr * expr * expr * expr * locatie
  | Skip of locatie

let rec string_of_expr = function
  | Int (i,_) -> string_of_int i
  | Bool (b,_) -> string_of_bool b
  | Float (f,_) -> string_of_float f
  | Loc (s,_) -> "!" ^ s
  | Op (e1, b, e2,_) ->
    "(" ^ (string_of_expr e1) ^ (string_of_op b) ^ (string_of_expr e2) ^ ")"
  | Atrib (s,e2,_) ->
    s ^ ":=" ^ (string_of_expr e2)
  | If (e1, e2, e3,_) ->
    "if " ^ (string_of_expr e1) ^ "\nthen " ^ (string_of_expr e2) ^ "\nelse (" ^ (string_of_expr e3) ^ ")"
  | While (e1, e2,_) ->
    "while " ^ (string_of_expr e1) ^ " do \n" ^ (string_of_expr e2) ^ "\ndone"
  | For (e1, e2, e3, e4, _)
    -> "for (" ^ (string_of_expr e1) ^ "; " ^ (string_of_expr e2) ^ "; " ^ (string_of_expr e3) ^ ") (\n" ^ (string_of_expr e2) ^ "\n)"
  | Secv (e1,e2,_) ->
    (string_of_expr e1) ^ ";\n" ^ (string_of_expr e2)
  | Skip _ -> "skip"


let location = function
  | Int (_,l)
  | Bool (_,l)
  | Float (_,l)
  | Loc (_,l)
  | Op (_, _, _,l)
  | Atrib (_,_,l)
  | If (_, _, _,l)
  | While (_, _,l)
  | For (_, _, _, _,l)
  | Secv (_,_,l)
  | Skip l 
  -> l


let rec insert x l = match l with
  | [] -> [x]
  | h::t when h = x -> l
  | h::t when x < h -> x::l
  | h::t -> h::(insert x t)

let locations e =
    let rec locations locs = function
      | [] -> locs
      | e::exps -> (match e with
          | Bool _ | Int _ | Float _ | Skip _ -> locations locs exps
          | Loc (l,_) -> locations (insert l locs) exps
          | Op(e1,_,e2,_) -> locations locs (e1::e2::exps)
          | Atrib(l,e,_) -> locations (insert l locs) (e::exps)
          | Secv(e1,e2,_) -> locations locs (e1::e2::exps)
          | If(e1,e2,e3,_) -> locations locs (e1::e2::e3::exps)
          | While(e1,e2,_) -> locations locs (e1::e2::exps)
          | For(e1,e2,e3,e4,_) -> locations locs (e1::e2::e3::e4::exps)
      )
    in locations [] [e]


