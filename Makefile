.PHONY: all test clean
all:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir schale.byte

test:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir test.byte
	./test.byte

web:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir -plugin-tag "package(js_of_ocaml.ocamlbuild)" sh.js

clean:
	ocamlbuild -clean
