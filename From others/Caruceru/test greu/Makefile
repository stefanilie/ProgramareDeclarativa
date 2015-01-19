
OS := $(shell uname)
ifneq (,$(findstring mingw,$(OS)))
  OUT := imp.exe
else
  ifneq (,$(findstring MINGW,$(OS)))
    OUT := imp.exe
  else
    ifneq (,$(findstring win,$(OS)))
      OUT := imp.exe
    else
      ifneq (,$(findstring WIN,$(OS)))
        OUT := imp.exe
      else
        OUT := imp
      endif
    endif
  endif
endif

all: $(OUT)

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

$(OUT): impAST.cmo lexer.cmo parser.cmo imp.cmo mem.cmo semantics.cmo types.cmo
	ocamlc -o $(OUT) impAST.cmo lexer.cmo parser.cmo mem.cmo semantics.cmo types.cmo imp.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli *.cmo *.cmi $(OUT)
