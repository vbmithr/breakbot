CC=ocamlbuild
OPTS=-use-ocamlfind
TARGETS=breakbot.ml cli.ml

all: native

native:    $(TARGETS:.ml=.native)
byte:      $(TARGETS:.ml=.byte)
debug:     $(TARGETS:.ml=.d.byte)
profiling: $(TARGETS:.ml=.p.native)

cli.[nbdp]*: _tags cli.ml mtgox.ml intersango.ml
breakbot.[nbdp]*: _tags breakbot.ml mtgox.ml intersango.ml ecb.ml

mtgox.ml: sharedbuf.ml websocket.ml common.ml cohttp_utils.ml\
utils.ml lwt_utils.ml exchange.ml

intersango.ml: common.ml cohttp_utils.ml utils.ml\
lwt_utils.ml exchange.ml

%.native %.byte %.d.byte %.p.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
