CAMLP4=-pp "camlp4o pa_extend.cmo"
OCAMLC_OPTS=-package str,core_extended,async,seq,ocaml_bio

tests=async_cmd_test pm_file_test

sources=global_state.ml m_rewrite_fasta.ml async_cmd.ml copy_file.ml \
	pm_file.ml mugsy_guide_tree.ml pm_job.ml \
	pm_sge_server.ml

sources_mli=global_state.mli m_rewrite_fasta.mli async_cmd.mli \
	pm_file.mli mugsy_guide_tree.mli pm_job.mli \
	pm_sge_server.mli

native_cmx=$(sources:%.ml=%.cmx)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native-code byte-code debug-code test clean

all: native-code

native-code: $(native_cmx)
	ocamlfind ocamlopt $(OCAMLC_OPTS) -thread -linkpkg -o paramugsy $(native_cmx)


test: $(tests)
	./async_cmd_test
	./pm_file_test

async_cmd_test: async_cmd_test.cmx
	ocamlfind ocamlopt -package core_extended,async -thread -linkpkg \
	-o async_cmd_test async_cmd.cmx async_cmd_test.cmx

async_cmd_test.cmx: async_cmd_test.ml async_cmd.cmx

pm_file_test: pm_file_test.cmx pm_file.cmx pm_file.cmi
	ocamlfind ocamlopt -package core_extended,oUnit -thread -linkpkg \
	-o pm_file_test pm_file.cmx pm_file_test.cmx

pm_file_test.cmx: pm_file_test.ml

clean:
	-rm paramugsy $(tests) *.cmi *.cmx *.cmo *.o


# Dependencies
pm_sge_server.cmx: pm_sge_server.cmi

async_cmd.cmx: async_cmd.cmi

pm_file.cmx: pm_file.cmi

mugsy_guide_tree.cmx: mugsy_guide_tree.cmi

global_state.cmx: global_state.cmi

m_rewrite_fasta.cmx: m_rewrite_fasta.cmi

pm_job.cmx: pm_job.cmi


-include ../Ocamlrules.mk.in
