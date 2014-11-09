CC=ocamlbuild
OPTS=-use-ocamlfind
TARGETS=src/breakbot.ml src/cli.ml

.PHONY: all native byte debug profiling cli.* breakbot.* clean

all: native

native:    $(TARGETS:.ml=.native)
byte:      $(TARGETS:.ml=.byte)
debug:     $(TARGETS:.ml=.d.byte)
profiling: $(TARGETS:.ml=.p.native)

cli.[nbdp]*:
	$(CC) $(OPTS) $@

breakbot.[nbdp]*:
	$(CC) $(OPTS) $@

%.native %.byte %.d.byte %.p.native: %.ml
	$(CC) $(OPTS) $@

clean:
	$(CC) -clean
