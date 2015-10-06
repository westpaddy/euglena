.PHONY: all test clean
all:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir schale.byte
test:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir test.byte
	./test.byte
clean:
	ocamlbuild -clean
