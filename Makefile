CC=ocamlbuild
OPTS=-use-ocamlfind
TARGETS=breakbot.ml cli.ml

all: native

native:    $(TARGETS:.ml=.native)
byte:      $(TARGETS:.ml=.byte)
debug:     $(TARGETS:.ml=.d.byte)
profiling: $(TARGETS:.ml=.p.native)

cli.[nbdp]*: alldeps
breakbot.[nbdp]*: alldeps

alldeps: _tags cli.ml breakbot.ml mtgox.ml intersango.ml ecb.ml\
sharedbuf.ml websocket.ml mycohttp.ml utils.ml lwt_utils.ml\
common.ml config.ml

%.native %.byte %.d.byte %.p.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
