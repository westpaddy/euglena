.PHONY: all clean
all:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir schale.byte
clean:
	ocamlbuild -clean
