OCAMLC = ocamlc
OUT = mycc
FLAGS = -I src

all:
	$(OCAMLC) $(FLAGS) -c src/ast.ml
	$(OCAMLC) $(FLAGS) -c src/lexer.ml
	$(OCAMLC) $(FLAGS) -c src/parser.ml
	$(OCAMLC) $(FLAGS) -c src/tacky.ml
	$(OCAMLC) $(FLAGS) -c src/tackygen.ml
	$(OCAMLC) $(FLAGS) -c src/asm.ml
	$(OCAMLC) $(FLAGS) -c src/codegen.ml
	$(OCAMLC) $(FLAGS) -c src/emit.ml
	$(OCAMLC) $(FLAGS) -c src/driver.ml
	$(OCAMLC) $(FLAGS) -o $(OUT) src/ast.cmo src/lexer.cmo src/parser.cmo src/tacky.cmo src/tackygen.cmo src/asm.cmo src/codegen.cmo src/emit.cmo src/driver.cmo

clean:
	rm -f src/*.cmi src/*.cmo $(OUT) out.s