CAMLP4=-pp "camlp4o pa_extend.cmo"
OCAMLC_OPTS=-package str,core_extended,async,seq,ocaml_bio

tests=async_cmd_test pm_file_test queue_server_test

sources=global_state.ml m_rewrite_fasta.ml async_cmd.ml copy_file.ml \
	pm_file.ml mugsy_guide_tree.ml pm_job.ml \
	queue_job.ml sge_interface.ml queue_driver.ml \
	queue_server.ml pm_sge_main.ml paramugsy.ml

sources_mli=global_state.mli m_rewrite_fasta.mli async_cmd.mli \
	pm_file.mli mugsy_guide_tree.mli pm_job.mli \
	queue_job.mli sge_interface.mli queue_driver.mli \
	queue_server.mli

native_cmx=$(sources:%.ml=%.cmx)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native-code byte-code debug-code test clean

all: paramugsy

native-code: paramugsy

.deps: $(sources) $(sources_mli)
	ocamlfind ocamldep $(OCAMLC_OPTS) $(CAMLP4) $(sources) $(sources_mli) > .deps

paramugsy: $(native_cmx)
	ocamlfind ocamlopt $(OCAMLC_OPTS) -thread -linkpkg -o paramugsy $(native_cmx)

test: $(tests)
	./async_cmd_test
	./pm_file_test
	./queue_server_test

async_cmd_test: async_cmd_test.cmx
	ocamlfind ocamlopt -package core_extended,async -thread -linkpkg \
	-o async_cmd_test async_cmd.cmx async_cmd_test.cmx

async_cmd_test.cmx: async_cmd_test.ml async_cmd.cmx

pm_file_test: pm_file_test.cmx pm_file.cmx pm_file.cmi
	ocamlfind ocamlopt -package core_extended,oUnit -thread -linkpkg \
	-o pm_file_test pm_file.cmx pm_file_test.cmx

pm_file_test.cmx: pm_file_test.ml

queue_server_test: queue_server_test.cmx queue_server.cmx queue_server.cmi \
	queue_job.cmx pm_file.cmx pm_file.cmi
	ocamlfind ocamlopt -package str,core_extended,async,seq -thread -linkpkg \
	-o queue_server_test \
	pm_file.cmx queue_job.cmx queue_server.cmx queue_server_test.cmx

queue_server_test.cmx: queue_server.cmx queue_server.cmi

clean:
	-rm paramugsy $(tests) *.cmi *.cmx *.cmo *.o .deps


# Dependencies
include ../Ocamlrules.mk.in

# This won't exist until it is generated, so .deps needs to run first
-include .deps
