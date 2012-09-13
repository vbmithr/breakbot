CC=ocamlbuild
OPTS=-use-ocamlfind

all: intersango.native

intersango.native: intersango.ml intersango_common.ml intersango_parser.ml
	$(CC) $(OPTS) $@

%.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
