# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ounit lablgtk ocamlbuild"
	 
case "$OCAML_VERSION,$OPAM_VERSION" in
4.04.0,1.2.2) ppa=avsm/ocaml44+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac
	 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init
opam switch 4.04.0
opam install ${OPAM_DEPENDS}

eval `opam config env`
make
#make test
