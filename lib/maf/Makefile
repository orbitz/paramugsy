CAMLP4=
OCAMLC_OPTS=-package core
OCAMLOPT_OPTS=-for-pack Maf $(OCAMLC_OPTS)

cources=sequence.ml alignment.ml reader.ml writer.ml missing_seqs.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code clean

all: native byte

native-code: native

byte-code: byte

native: maf.cmxa

maf.cmxa: maf.cmx

maf.cmx: $(native_cmx)
	ocamlfind ocamlopt -pack -o $@ $^

byte: maf.cma

maf.cma: maf.cmo

maf.cmo: $(byte_cmo)
	ocamlfind ocamlc -pack -o $@ $^

clean:
	-rm *.cmxa *.cmi *.cmx *.cmo *.o .d

# Dependencies
include ../Ocamlrules.mk.in
