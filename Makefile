CC=ocamlbuild
OPTS=-use-ocamlfind
TARGETS=intersango.ml websocket.ml btce.ml

all: native

native:    $(TARGETS:.ml=.native)
byte:      $(TARGETS:.ml=.byte)
debug:     $(TARGETS:.ml=.d.byte)
profiling: $(TARGETS:.ml=.p.native)

btce.[nbdp]*: btce.ml common.ml utils.ml
websocket.[nbdp]*: websocket.ml common.ml utils.ml
intersango.[nbdp]*: intersango.ml intersango_common.ml \
	            intersango_parser.ml common.ml utils.ml

%.native %.byte %.d.byte %.p.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
