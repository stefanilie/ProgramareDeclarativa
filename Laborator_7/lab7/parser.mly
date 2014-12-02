/* File parser.mly */
%{
open ImpAST
open Lexing

let location () =  let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    Printf.sprintf "%s:%d.%d-%d.%d"
      start_pos.pos_fname
      start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
      end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)

let parseError loc = raise (Lexer.ParseError loc)

%}
%token <int> INT
%token <string> LOC
%token TRUE FALSE
%token SEQ SKIP
%token IF THEN ELSE
%token WHILE DO DONE
%token LTE
%token ASGNOP DEREF
%token PLUS
%token MINUS
/* pt ca lexeru cand analizeaza codul sa stie de tokenuri */
%token LPAREN RPAREN
%token EOF
%right SEQ /* lowest precedence */
%nonassoc IFX
%nonassoc LTE
%right ASGNOP
%left PLUS
%nonassoc DEREF       /* highest precedence */
%start main             /* the entry point */
%type <ImpAST.expr> main
%%
main:
    expr EOF                { $1 }
;
expr:
    INT                        { Int ($1,location()) }
  | TRUE                       { Bool (true, location()) }
  | FALSE                      { Bool (false, location()) }
  | SKIP                       { Skip (location()) }
  | LPAREN expr RPAREN         { $2 }
  | expr PLUS expr             { Op ($1,Plus,$3, location()) }
  | expr MINUS expr            { Op ($1,Minus,$3, location()) }
  /* $1 tine locul lui expr, $2 lui MINUS si $3 lui expr. 
  Location va retine locatia in cod in eventualitatea in care va aparea o eroare. */
  | DEREF LOC                  { Loc ($2, location()) }
  | LOC ASGNOP expr            { Atrib ($1,$3, location()) }
  | expr LTE expr              { Op ($1, Mic, $3, location()) }
  | expr SEQ expr              { Secv ($1,$3, location()) }
  | IF expr THEN expr ELSE expr %prec IFX
                               { If ($2, $4, $6, location()) }
  | WHILE expr DO expr DONE    { While ($2, $4, location()) }
  | error                      { parseError (location ()) }
;
