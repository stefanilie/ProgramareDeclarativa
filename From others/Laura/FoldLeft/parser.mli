type token =
  | INT_CAST
  | FLOAT_CAST
  | INT of (int)
  | FLOAT of (float)
  | VAR of (string)
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
  | FOR
  | LT
  | LTE
  | EQ
  | ASGNOP
  | DEREF
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LPAREN
  | RPAREN
  | LAC
  | RAC
  | FUN
  | COLON
  | LET
  | REC
  | IN
  | REF
  | TINT
  | TBOOL
  | TUNIT
  | TFLOAT
  | ARROW
  | PCT
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ImpAST.expr
