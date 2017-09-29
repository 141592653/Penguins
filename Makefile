PACKAGES=-package lablgtk2 -package oUnit
BUILD=ocamlbuild \
	$(PACKAGES)

default:
	$(BUILD) src/penguins.native
