PACKAGES=-package lablgtk2 -package oUnit
BUILD=ocamlbuild \
	$(PACKAGES)

default:
	$(BUILD) src/penguins.native

test:
	$(BUILD) src/test.native

doc:
	$(BUILD) src/penguins.docdir/index.html
