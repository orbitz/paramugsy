OCAMLPATH:=$(PWD)/src:$(OCAMLPATH)

.PHONY: all clean \
	setup core-install get-deps get-ocaml-bio get-ocaml-seq \
	clean-deps

all:
	$(MAKE) -C src

setup: get-deps

get-deps: core-install get-ocaml-bio get-ocaml-seq

core-install:
	ocaml setup/odb.ml --package $(PWD)/setup/core.odb.pkg

get-ocaml-bio:
	cd src && git clone git@github.com:orbitz/ocaml-bio.git

get-ocaml-seq:
	cd src && git clone git@github.com:orbitz/ocaml-seq.git

clean-deps:
	rm -rf src/ocaml-bio
	rm -rf src/ocaml-seq

clean:
	$(MAKE) -C src clean

