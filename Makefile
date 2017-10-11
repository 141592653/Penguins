PACKAGES=-package lablgtk2 -package oUnit -package yojson
BUILD=ocamlbuild \
	$(PACKAGES)

.PHONY: default test doc clean mrproper
default:
	$(BUILD) src/penguins.native

test:
	$(BUILD) src/test.native
	./test.native

doc:
	$(BUILD) -ocamldoc "ocamldoc -charset utf8" \
		src/penguins.docdir/index.html
	mv _build/src/penguins.docdir doc
	rm -rf penguins.docdir

clean:
	rm -rf _build

mrproper: clean
	rm -rf test.native penguins.native
