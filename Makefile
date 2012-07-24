OCAMLPATH:=$(PWD)/src:$(OCAMLPATH)

.PHONY: all native byte debug clean \
	setup core-install get-deps get-ocaml-bio get-ocaml-seq \
	clean-deps

all: native

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

native:
	$(MAKE) -C src native

byte:
	$(MAKE) -C src byte

debug:
	$(MAKE) -C src debug

clean:
	$(MAKE) -C src clean

