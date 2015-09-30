.PHONY: all clean
all:
	ocamlbuild -j 0 -use-ocamlfind -use-menhir schale.native
clean:
	ocamlbuild -clean
