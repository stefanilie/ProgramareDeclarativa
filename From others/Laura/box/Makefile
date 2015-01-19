

all: imp

opt:
	ocamlopt -c impAST.ml
	ocamlyacc parser.mly
	ocamlopt -c parser.mli
	ocamllex lexer.mll
	ocamlopt -c lexer.ml
	ocamlopt -c parser.ml
	ocamlopt -c mem.ml
	ocamlopt -c semantics.ml
	ocamlopt -c types.ml
	ocamlopt -c imp.ml
	ocamlopt -o imp-opt impAST.cmx lexer.cmx parser.cmx mem.cmx semantics.cmx types.cmx imp.cmx

impAST.cmo : impAST.ml
	ocamlc -c impAST.ml

mem.cmo: mem.ml impAST.cmo
	ocamlc -c mem.ml

lexer.ml: lexer.mll parser.cmi
	ocamllex lexer.mll

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

parser.ml: parser.mly impAST.cmo
	ocamlyacc parser.mly

parser.cmi: parser.ml
	ocamlc -c parser.mli

parser.cmo: parser.ml impAST.cmo
	ocamlc -c parser.ml

types.cmo: types.ml mem.cmo impAST.cmo
	ocamlc -c types.ml

semantics.cmo: semantics.ml mem.cmo impAST.cmo
	ocamlc -c semantics.ml

imp.cmo: imp.ml semantics.cmo types.cmo lexer.cmo
	ocamlc -c imp.ml

imp: impAST.cmo lexer.cmo parser.cmo imp.cmo mem.cmo semantics.cmo types.cmo
	ocamlc -o imp impAST.cmo lexer.cmo parser.cmo mem.cmo semantics.cmo types.cmo imp.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli *.cmo *.cmi imp
	rm -f *.cmx *.o
