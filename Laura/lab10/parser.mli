type token =
  | Z
  | INT_CAST
  | FLOAT_CAST
  | INT of (int)
  | FLOAT of (float)
  | LOC of (string)
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
  | FUN
  | COLON
  | TINT
  | TBOOL
  | TUNIT
  | TFLOAT
  | ARROW
  | FUNX
  | EOF
  | IN
  | LET
  | LETX

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ImpAST.expr
