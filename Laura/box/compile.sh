ocamlc -c impAST.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamllex lexer.mll
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c mem.ml
ocamlc -c semantics.ml
ocamlc -c types.ml
ocamlc -c imp.ml
ocamlc -o imp impAST.cmo lexer.cmo parser.cmo mem.cmo semantics.cmo types.cmo imp.cmo
