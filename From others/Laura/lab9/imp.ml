open Lexing
open Types
open ImpAST

let is_float_name s = String.length s > 1 && String.get s 1 = 'f'

let init_type s = if is_float_name s then TFloatRef else TIntRef
let init_val s = if is_float_name s then Float (0., "mem") else Int (0,"mem")

let rec init_mtype = function
  | [] -> []
  | h::t -> (h,init_type h)::(init_mtype t)

let rec init_mem = function
  | [] -> []
  | h::t -> (h,init_val h)::(init_mem t)

let type_and_run debug pgm =
  let locs = ImpAST.locations pgm in
      if (type_check  (init_mtype locs) pgm) then begin
          Printf.printf "The program typechecks. Executing..." ;
          flush stdout ;
          let final = (Semantics.evaluate debug (pgm,init_mem locs)) in
            Printf.printf " done.\n Final configuration:\n\n  %s\n\n" 
                  (Semantics.string_of_config final) 
      end else ()


let cin () =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else failwith "please specify program file"

let _ = 
  let lexbuf = Lexing.from_channel (cin ()) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Sys.argv.(1) };
    try 
      let pgm = Parser.main Lexer.token lexbuf in
       type_and_run (Array.length Sys.argv > 2) pgm
    with
      Lexer.LexerError loc ->
        Printf.eprintf "Lexer Error: Unexpected token '%s' at %s\n" (Lexing.lexeme lexbuf) loc
    | Lexer.ParseError loc ->
        Printf.eprintf "Parse Error: Unexpected token '%s' at %s\n" (Lexing.lexeme lexbuf) loc
