PACKAGES=-package lablgtk2 -package oUnit
BUILD=ocamlbuild \
	$(PACKAGES)

default:
	$(BUILD) src/penguins.native

test:
	$(BUILD) src/test.native

doc:
	$(BUILD) -ocamldoc "ocamldoc -charset utf8" src/penguins.docdir/index.html

clean:
	rm -rf _build
