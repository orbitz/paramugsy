OCAMLPATH:=$(PWD)/src:$(OCAMLPATH)

.PHONY: all test clean \
	setup core-install get-deps get-ocaml-bio get-ocaml-seq \
	clean-deps

all:
	$(MAKE) -C src

test:
	$(MAKE) -C src test

setup: get-deps

get-deps: get-ocaml-bio get-ocaml-seq

get-ocaml-bio:
	cd src && git clone https://github.com/orbitz/ocaml-bio.git

get-ocaml-seq:
	cd src && git clone https://github.com/orbitz/ocaml-seq.git

clean-deps:
	rm -rf src/ocaml-bio
	rm -rf src/ocaml-seq

clean:
	$(MAKE) -C src clean

