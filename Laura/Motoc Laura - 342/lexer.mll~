(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)

(**
  Type of exception used for lexer errors.
  Parameter:  location in the file.
 *)
exception LexerError of ImpAST.locatie
(**
  Type of exception used for parser errors.
  Parameter:  location in the file
 *)
exception ParseError of ImpAST.locatie

(**  
  Incrementing line numbers (at line breaks) while tokenizing
  @param lexbuf lexer internal object of Lexing.lexbuf type
  @see Lexing#lexbuf and Lexing#position
 *)
let incr_linenum lexbuf =
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with
        (* incrementing line number *)
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        (* setting offset for beggining of line *)
        Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

(**
  Formats an error string at the location given by current tokenizing position.
  @param lexbuf lexer internal object of Lexing.lexbuf type
  @see Lexing#lexbuf and Lexing#position
 *)
let lex_error lexbuf =
         begin
           let curr = lexbuf.Lexing.lex_curr_p in
           let file = curr.Lexing.pos_fname
           and line = curr.Lexing.pos_lnum
           and cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
              raise (LexerError (file,line,cnum,line,cnum))
         end

(**
  A hash table associating symbol constants to keywords.
  *)
let keyword_table = Hashtbl.create 20   
                   (* create an hashtable with initial capacity 20 *)
let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    (* insert each keyyword-symbol pair in the list into the hashtable *)
  [
   ( "fun"          , FUN );
   ( "int"          , TINT );
   ( "bool"         , TBOOL );
   ( "float"        , TFLOAT );
   ( "unit"         , TUNIT );
   ( "int_of_float" , INT_CAST );
   ( "float_of_int" , FLOAT_CAST );
   ( "if"           , IF );
   ( "then"         , THEN );
   ( "else"         , ELSE );
   ( "while"        , WHILE );
   ( "do"           , DO );
   ( "done"         , DONE );
   ( "for"          , FOR );
   ( "true"         , TRUE );
   ( "false"        , FALSE );
   ( "let"          , LET );
   ( "rec"          , REC );
   ( "in"           , IN );
   ( "ref"          , REF );
]

}

(* rule for associating symbols to keywords/symbols in the program text *)
rule token = parse
    [ '\n' ] { incr_linenum lexbuf ; token lexbuf } 
     (* skip line breaks, but first increment line number *)
  | "(*"            { comments 0 lexbuf }
     (* implementing ocaml-style comments -- see comments rule below *)
  | [' ' '\t' '\r' ]     { token lexbuf }     (* skip blanks *)
  | ['-']?['0'-'9']+['.']['0'-'9']* as lxm { FLOAT(float_of_string lxm) }
    (* regular expression for floating point numbers:
         may start (or not - ?)  with minus  
         followed by at least one (but maybe more - +) digits
         followed by (exactly) one dot
         followed by (0,1, or more - * ) digits for the fractional part *)
  | ['-']?['0'-'9']+ as lxm { INT(int_of_string lxm) }
    (* regular expression for integer numbers:
         may start (or not - ?)  with minus  
         followed by at least one (but maybe more - +) digits *)
  | "->"           { ARROW } 
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { MUL }
  | '/'            { DIV }
  | '<'            { LT }
  | "<="           { LTE }
  | "()"           { SKIP }  (* empty stmt is defined by () to match ocaml *)
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LAC }        (*Adaugat*)
  | '}'            { RAC }		(*Adaugat*)
  | "="            { EQ }
  | ":="           { ASGNOP }
  | ":"            { COLON }
  | ';'            { SEQ }
  | '!'            { DEREF }
  | '.'            { PCT }   (*ADAUGAT*)
  (*  next is a list of non-keyword symbols we want our language to have *)
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                   { try Hashtbl.find keyword_table id 
                        (* if it's a keyword, use the symbol from the table *)
                     with Not_found -> VAR(id) 
                        (* if not, consider it a variable *)
                   }
  | eof            { EOF }
  | _              { lex_error lexbuf }
(* comments rule takes an additional integer argument level for the level of
comment nesting to allow comments inside comments like this (* efhi *) *)
and comments level = parse
  | "*)"        { if level = 0 then token lexbuf
                  else comments (level-1) lexbuf }
     (* if level is 0, then we're closing the comment, else decrement level *)
  | "(*"        { comments (level+1) lexbuf }
     (* nested comment - increment comment level *)
  | [ '\n' ]    { incr_linenum lexbuf ; comments level lexbuf }
                (* skip line breaks, but still increment lines *)
  | _           { comments level lexbuf }
    (* skip everything else inside the comment *)
  | eof         { raise End_of_file }
