CC=ocamlbuild
OPTS=-use-ocamlfind

all: intersango.native websocket.native

websocket.native: websocket.ml common.ml utils.ml

intersango.native: intersango.ml intersango_common.ml intersango_parser.ml common.ml utils.ml
	$(CC) $(OPTS) $@

%.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
