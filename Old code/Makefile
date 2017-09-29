# OCAMLC = ocamlopt -noassert -unsafe
OCAMLC = ocamlopt -g

ML=$(shell ocamldep -sort $(wildcard *.ml))
MODULES=$(ML:.ml=.cmx)

TEX=$(wildcard *.tex)
PDF=$(TEX:.tex=.pdf)

default: $(MODULES) pingouin $(PDF)

pingouin: $(MODULES)
	if [ -f main.ml ] ; then \
	  $(OCAMLC) $(MODULES) -o pingouin ; \
	fi

SOURCES = $(wildcard *.ml) $(wildcard *mli)
.depend: $(SOURCES)
	ocamldep $(SOURCES) > .depend
-include .depend

%.cmx: %.ml Makefile
	$(OCAMLC) -c $<
%.cmi: %.mli Makefile
	$(OCAMLC) -c $<

tarball:
	rm -rf prog2_pingouin
	mkdir prog2_pingouin
	cp *.mli Makefile prog2_pingouin
	mkdir prog2_pingouin/problems
	cp problems/* prog2_pingouin/problems
	tar zcvf prog2_pingouin.tar.gz prog2_pingouin/

clean:
	rm -f pingouin
	rm -f *.cmx *.cmo *.cmi *.o

%.pdf: %.tex
	rubber -d $<
