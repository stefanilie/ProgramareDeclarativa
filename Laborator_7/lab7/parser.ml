type token =
  | INT of (int)
  | LOC of (string)
  | TRUE
  | FALSE
  | SEQ
  | SKIP
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | DONE
  | LTE
  | ASGNOP
  | DEREF
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
# 2 "parser.mly"

open ImpAST
open Lexing

let location () =  let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    Printf.sprintf "%s:%d.%d-%d.%d"
      start_pos.pos_fname
      start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
      end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)

let parseError loc = raise (Lexer.ParseError loc)

# 41 "parser.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* SEQ *);
  262 (* SKIP *);
  263 (* IF *);
  264 (* THEN *);
  265 (* ELSE *);
  266 (* WHILE *);
  267 (* DO *);
  268 (* DONE *);
  269 (* LTE *);
  270 (* ASGNOP *);
  271 (* DEREF *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* MUL *);
  275 (* DIV *);
  276 (* LPAREN *);
  277 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* LOC *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\002\000\003\000\003\000\003\000\006\000\005\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\017\000\002\000\000\000\003\000\004\000\005\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\016\000\
\000\000"

let yydgoto = "\002\000\
\013\000\014\000"

let yysindex = "\255\255\
\030\255\000\000\000\000\000\000\249\254\000\000\000\000\000\000\
\030\255\030\255\010\255\030\255\000\000\077\000\030\255\056\255\
\065\255\000\000\039\255\030\255\030\255\030\255\030\255\030\255\
\030\255\000\000\109\255\030\255\030\255\000\000\098\255\109\255\
\241\254\247\254\250\254\000\000\080\255\089\255\030\255\000\000\
\105\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\000\000\000\000\000\000\000\068\000\054\000\
\029\000\015\000\001\000\000\000\000\000\000\000\000\000\000\000\
\062\000"

let yygindex = "\000\000\
\000\000\252\255"

let yytablesize = 352
let yytable = "\001\000\
\009\000\023\000\024\000\025\000\016\000\017\000\015\000\019\000\
\024\000\025\000\027\000\018\000\025\000\000\000\008\000\031\000\
\032\000\033\000\034\000\035\000\036\000\000\000\000\000\037\000\
\038\000\000\000\000\000\000\000\007\000\003\000\004\000\005\000\
\006\000\007\000\041\000\008\000\009\000\000\000\000\000\010\000\
\000\000\000\000\012\000\020\000\011\000\000\000\000\000\000\000\
\000\000\012\000\000\000\021\000\000\000\013\000\022\000\023\000\
\024\000\025\000\000\000\030\000\020\000\015\000\000\000\028\000\
\000\000\000\000\000\000\014\000\021\000\020\000\000\000\022\000\
\023\000\024\000\025\000\029\000\026\000\021\000\000\000\000\000\
\022\000\023\000\024\000\025\000\020\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\021\000\020\000\000\000\022\000\
\023\000\024\000\025\000\000\000\040\000\021\000\020\000\000\000\
\022\000\023\000\024\000\025\000\000\000\000\000\021\000\000\000\
\000\000\022\000\023\000\024\000\025\000\021\000\000\000\000\000\
\022\000\023\000\024\000\025\000\022\000\023\000\024\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\009\000\009\000\000\000\009\000\009\000\009\000\000\000\000\000\
\009\000\009\000\009\000\008\000\000\000\009\000\008\000\008\000\
\000\000\008\000\008\000\008\000\000\000\000\000\008\000\008\000\
\000\000\007\000\000\000\008\000\007\000\007\000\000\000\007\000\
\007\000\007\000\000\000\000\000\007\000\000\000\000\000\012\000\
\000\000\007\000\012\000\012\000\000\000\012\000\012\000\012\000\
\000\000\000\000\013\000\000\000\000\000\013\000\013\000\012\000\
\013\000\013\000\015\000\000\000\000\000\015\000\015\000\000\000\
\015\000\015\000\013\000\014\000\014\000\000\000\014\000\014\000\
\000\000\020\000\015\000\000\000\000\000\000\000\000\000\000\000\
\014\000\021\000\000\000\000\000\022\000\023\000\024\000\025\000"

let yycheck = "\001\000\
\000\000\017\001\018\001\019\001\009\000\010\000\014\001\012\000\
\018\001\019\001\015\000\002\001\019\001\255\255\000\000\020\000\
\021\000\022\000\023\000\024\000\025\000\255\255\255\255\028\000\
\029\000\255\255\255\255\255\255\000\000\000\001\001\001\002\001\
\003\001\004\001\039\000\006\001\007\001\255\255\255\255\010\001\
\255\255\255\255\000\000\005\001\015\001\255\255\255\255\255\255\
\255\255\020\001\255\255\013\001\255\255\000\000\016\001\017\001\
\018\001\019\001\255\255\021\001\005\001\000\000\255\255\008\001\
\255\255\255\255\255\255\000\000\013\001\005\001\255\255\016\001\
\017\001\018\001\019\001\011\001\000\000\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\005\001\255\255\255\255\255\255\
\009\001\255\255\255\255\255\255\013\001\005\001\255\255\016\001\
\017\001\018\001\019\001\255\255\012\001\013\001\005\001\255\255\
\016\001\017\001\018\001\019\001\255\255\255\255\013\001\255\255\
\255\255\016\001\017\001\018\001\019\001\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\016\001\017\001\018\001\019\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\255\255\255\255\
\008\001\009\001\255\255\011\001\012\001\013\001\255\255\255\255\
\016\001\017\001\018\001\005\001\255\255\021\001\008\001\009\001\
\255\255\011\001\012\001\013\001\255\255\255\255\016\001\017\001\
\255\255\005\001\255\255\021\001\008\001\009\001\255\255\011\001\
\012\001\013\001\255\255\255\255\016\001\255\255\255\255\005\001\
\255\255\021\001\008\001\009\001\255\255\011\001\012\001\013\001\
\255\255\255\255\005\001\255\255\255\255\008\001\009\001\021\001\
\011\001\012\001\005\001\255\255\255\255\008\001\009\001\255\255\
\011\001\012\001\021\001\008\001\009\001\255\255\011\001\012\001\
\255\255\005\001\021\001\255\255\255\255\255\255\255\255\255\255\
\021\001\013\001\255\255\255\255\016\001\017\001\018\001\019\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  SEQ\000\
  SKIP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  DONE\000\
  LTE\000\
  ASGNOP\000\
  DEREF\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  LOC\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                            ( _1 )
# 238 "parser.ml"
               : ImpAST.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 47 "parser.mly"
                               ( Int (_1,location()) )
# 245 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                               ( Bool (true, location()) )
# 251 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                               ( Bool (false, location()) )
# 257 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                               ( Skip (location()) )
# 263 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                               ( _2 )
# 270 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                               ( Op (_1,Plus,_3, location()) )
# 278 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                               ( Op (_1,Minus,_3, location()) )
# 286 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                               ( Op (_1,Mul,_3, location()) )
# 294 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                               ( Op (_1,Div,_3, location()) )
# 302 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                               ( Loc (_2, location()) )
# 309 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                               ( Atrib (_1,_3, location()) )
# 317 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                               ( Op (_1, Mic, _3, location()) )
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                               ( Secv (_1,_3, location()) )
# 333 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                               ( If (_2, _4, _6, location()) )
# 342 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                               ( While (_2, _4, location()) )
# 350 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                               ( parseError (location ()) )
# 356 "parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ImpAST.expr)
