ocamlc -c impAST.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlyacc parser.mly
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c parser.mli
@if ERRORLEVEL 1 EXIT /B 1
ocamllex lexer.mll
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c lexer.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c parser.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c mem.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c semantics.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c types.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -c imp.ml
@if ERRORLEVEL 1 EXIT /B 1
ocamlc -o imp.exe impAST.cmo lexer.cmo parser.cmo mem.cmo semantics.cmo types.cmo imp.cmo
@if ERRORLEVEL 1 EXIT /B 1
