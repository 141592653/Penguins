PACKAGES=-package lablgtk2 -package oUnit -package yojson
BUILD=ocamlbuild -use-ocamlfind -tag thread \
	-cflags "-g -w +A-4-48"\
	$(PACKAGES)

.PHONY: default test doc clean mrproper
default:
	$(BUILD) src/penguins.native

test:
	$(BUILD) src/test.native
	./test.native

doc:
	$(BUILD) -ocamldoc "ocamldoc -charset utf8 -stars" \
		src/penguins.docdir/index.html
	rm -rf doc
	mv _build/src/penguins.docdir doc
	rm -rf penguins.docdir

clean:
	rm -rf _build

mrproper: clean
	rm -rf test.native penguins.native

