type token =
  | INT_CAST
  | FLOAT_CAST
  | INT of (int)
  | FLOAT of (float)
  | VAR of (string)
  | R_INT
  | P_INT
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
  | WHEN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LPAREN
  | RPAREN
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
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ImpAST.expr
