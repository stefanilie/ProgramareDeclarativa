(** Entry point for the imp interpreter *)
open Lexing
open Types
open ImpAST

(** Typechecks [pgm], and if scuccessful, executes it
    and prints its final configuration.
    @param debug  whether intermediate configurations should be displayed.
    @param pgm the program to be executed.
   *)
let type_and_run (debug:bool) (pgm:expr) : unit =
      if (type_check pgm) then begin
          Printf.printf "The program typechecks. Executing..." ;
          if debug then print_newline () else () ;
          flush stdout ;
          let final = (Semantics.evaluate debug (pgm,[])) in
            Printf.printf " done.\n Final configuration:\n\n  %s\n\n"
                  (Semantics.string_of_config final)
      end else ()


(** Opens the program specified as the first argument in the command line
    and returns a channel to it. *)
let cin () =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else failwith "please specify program file"

(** Main function. Calls the lexer and the parser to obtain an expression
    from the given program file, then calls [type_and_run] on it.
    Formats lexing and parsing exceptions. *)
let _ =
  let lexbuf = Lexing.from_channel (cin ()) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Sys.argv.(1) };
    try
      let pgm = Parser.main Lexer.token lexbuf in
       type_and_run (Array.length Sys.argv > 2) pgm
    with
      Lexer.LexerError (f,l,c,_,_) ->
        Printf.eprintf "%s:%d.%d\nLexer Error: Unexpected token '%s'\n" f l c (Lexing.lexeme lexbuf) 
    | Lexer.ParseError loc ->
        Printf.eprintf "%s\nParse Error: Unexpected token '%s'\n" (string_of_locatie loc) (Lexing.lexeme lexbuf) 
