open Lexing
open Types

let rec init_mem v = function
  | [] -> []
  | h::t -> (h,v)::(init_mem v t)

exception Error of exn * (string*int * int * string (** string *))


let type_and_run pgm = 
  let locs = ImpAST.locations pgm in
      if (type_check  (init_mem TIntRef locs) pgm) then begin
          Printf.printf "The program typechecks. Executing..." ;
          flush stdout ;
          let final = (Semantics.evaluate (pgm,init_mem 0 locs)) in
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
       type_and_run pgm
    with
      Lexer.LexerError (loc,tok) ->
        Printf.eprintf "Lexer Error: Unexpected token '%s' at %s\n" (Lexing.lexeme lexbuf) loc
    | Lexer.ParseError loc ->
        Printf.eprintf "Parse Error: Unexpected token '%s' at %s\n" (Lexing.lexeme lexbuf) loc
