CC=ocamlbuild
OPTS=-use-ocamlfind

all: intersango.native

%.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
