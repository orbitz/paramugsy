export OCAMLPATH:=$(PWD)/lib:$(OCAMLPATH)

.PHONY: all test clean \
	setup core-install get-deps get-ocaml-bio get-ocaml-seq \
	clean-deps

all:
	$(MAKE) -C lib

test:
	$(MAKE) -C lib test

setup: get-deps

get-deps: get-ocaml-bio get-ocaml-seq

get-ocaml-bio:
	cd lib && git clone https://github.com/orbitz/ocaml-bio.git

get-ocaml-seq:
	cd lib && git clone https://github.com/orbitz/ocaml-seq.git

clean-deps:
	rm -rf lib/ocaml-bio
	rm -rf lib/ocaml-seq

clean:
	$(MAKE) -C lib clean

