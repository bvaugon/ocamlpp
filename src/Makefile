###########################################################################
##                                                                       ##
##                               OCamlPP                                 ##
##                                                                       ##
##                            Benoit Vaugon                              ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../LICENSE-en.                                            ##
##                                                                       ##
###########################################################################

include ../etc/Makefile.conf

BIN = ../bin
SRCS = $(wildcard *.ml *.mli)
TARGS = $(BIN)/ocamlpp

all: $(TARGS)

$(BIN)/%: $(SRCS) config.ml
	$(OCAMLBUILD) -cflag -g -lflag -g $*.native
	cp _build/$*.native $@

config.ml: ../etc/config.ml
	cp $< $@

clean:
	@$(OCAMLBUILD) -clean
	@rm -f $(TARGS) config.ml

.PHONY: all clean
