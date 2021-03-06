OPAM_DEPENDS="ocamlfind ounit lablgtk ocamlbuild yojson"

# case "$OCAML_VERSION,$OPAM_VERSION" in
# 4.04.0,1.2.2) ppa=avsm/ocaml44+opam12 ;;
# *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
# esac

yes | opam init
yes | opam switch 4.04.0
opam install --yes ${OPAM_DEPENDS}

eval `opam config env`
make && make test && make doc
