CC=ocamlbuild
OPTS=-use-ocamlfind
TARGETS=breakbot.ml websocket.ml btce.ml

all: native

native:    $(TARGETS:.ml=.native)
byte:      $(TARGETS:.ml=.byte)
debug:     $(TARGETS:.ml=.d.byte)
profiling: $(TARGETS:.ml=.p.native)

btce.[nbdp]*: btce.ml common.ml utils.ml
websocket.[nbdp]*: websocket.ml common.ml utils.ml
breakbot.[nbdp]*: breakbot.ml intersango.ml common.ml utils.ml

%.native %.byte %.d.byte %.p.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
