CC=ocamlbuild
OPTS=-use-ocamlfind
TARGETS=breakbot.ml cli.ml

all: native

native:    $(TARGETS:.ml=.native)
byte:      $(TARGETS:.ml=.byte)
debug:     $(TARGETS:.ml=.d.byte)
profiling: $(TARGETS:.ml=.p.native)

cli.[nbdp]*: _tags cli.ml
btce.[nbdp]*: _tags btce.ml common.ml utils.ml lwt_utils.ml
breakbot.[nbdp]*: _tags ecb.ml sharedbuf.ml breakbot.ml \
websocket.ml mtgox.ml intersango.ml common.ml utils.ml lwt_utils.ml exchange.ml

%.native %.byte %.d.byte %.p.native: %.ml
	$(CC) $(OPTS) $@

.PHONY: clean

clean:
	$(CC) -clean
