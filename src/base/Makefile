CAMLP4=-pp "camlp4o pa_extend.cmo"
OCAMLC_OPTS=-package str,core_extended,async,seq,ocaml_bio

sources=global_state.ml m_rewrite_fasta.ml async_cmd.ml copy_file.ml \
	mugsy_guide_tree.ml pm_job.ml

sources_mli=global_state.mli m_rewrite_fasta.mli mugsy_guide_tree.mli \
	pm_job.mli async_cmd.mli

native_cmx=$(sources:%.ml=%.cmx)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native-code byte-code debug-code test clean

all: native-code

native-code: $(native_cmx)
	ocamlfind ocamlopt $(OCAMLC_OPTS) -thread -custom -linkpkg -o paramugsy $(native_cmx)

$(native_cmx): $(sources) $(native_cmi)

$(native_cmi): $(sources_mli)

test: async_cmd_test
	./async_cmd_test

async_cmd_test: async_cmd_test.cmx
	ocamlfind ocamlopt -package core_extended,async -thread -linkpkg \
	-o async_cmd_test async_cmd.cmx async_cmd_test.cmx

async_cmd_test.cmx: async_cmd_test.ml async_cmd.cmx


clean:
	-rm paramugsy async_cmd_test *.cmi *.cmx *.cmo *.o

-include ../Ocamlrules.mk.in
