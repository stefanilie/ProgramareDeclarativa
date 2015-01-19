(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)

exception LexerError of string * string
exception ParseError of string

(*  Code for maintaining line numbers while tokenizing *)

let incr_linenum lexbuf =
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with
        (* incrementing line number *)
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        (* setting offset for beggining of line *)
        Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

let lex_error lexbuf =
         begin
           let curr = lexbuf.Lexing.lex_curr_p in
           let file = curr.Lexing.pos_fname
           and line = curr.Lexing.pos_lnum
           and cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol
           and tok = Lexing.lexeme lexbuf in
              raise (LexerError (Printf.sprintf "%s:%d.%d" file line cnum,tok))
         end

let keyword_table = Hashtbl.create 20
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [( "if"           , IF );
   ( "then"         , THEN );
   ( "else"         , ELSE );
   ( "while"        , WHILE );
   ( "do"           , DO );
   ( "done"         , DONE );
   ( "for"          , FOR );
   ( "true"         , TRUE );
   ( "false"        , FALSE );
   ( "skip"         , SKIP )]

}

rule token = parse
    [ '\n' ] { incr_linenum lexbuf ; token lexbuf }
  | [' ' '\t' '\r' ]     { token lexbuf }     (* skip blanks *)
  | ['-']?['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['-']?['0'-'9']+['.']['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { MUL }
  | '/'            { DIV }
  | "<="           { LTE }
  | "=="           { EQ }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ":="           { ASGNOP }
  | ';'            { SEQ }
 | '!'            { DEREF }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                   { try Hashtbl.find keyword_table id 
                     with Not_found -> LOC(id)}
  | eof            { EOF }
  | _              { lex_error lexbuf }
