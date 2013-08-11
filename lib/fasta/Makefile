CAMLP4=
OCAMLC_OPTS=-package core
OCAMLOPT_OPTS=-for-pack Fasta $(OCAMLC_OPTS)

cources=reader.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code clean

all: native byte

native-code: native

byte-code: byte

native: fasta.cmxa

fasta.cmxa: fasta.cmx

fasta.cmx: $(native_cmx)
	ocamlfind ocamlopt -pack -o $@ $^

byte: fasta.cma

fasta.cma: fasta.cmo

fasta.cmo: $(byte_cmo)
	ocamlfind ocamlc -pack -o $@ $^

clean:
	-rm *.cmxa *.cmi *.cmx *.cmo *.o .d

# Dependencies
include ../Ocamlrules.mk.in
