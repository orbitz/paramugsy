CAMLP4=
OCAMLC_OPTS=-package maf,fasta
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

cources=

sources=$(cources) main.ml

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code clean

all: native byte

native-code: native

byte-code: byte

native: maffiller

maffiller: $(native_cmx)
	ocamlfind ocamlopt $(OCAMLOPT_OPTS) -thread -linkpkg -o $@ $^

byte: maffiller.byte

maffiller.byte: $(byte_cmo)
	ocamlfind ocamlc $(OCAMLC_OPTS) -thread -linkpkg -custom -o $@ $^

clean:
	-rm maffiller maffiller.byte *.cmxa *.cmi *.cmx *.cmo *.o .d

# Dependencies
include ../Ocamlrules.mk.in
