CAMLP4=-pp "camlp4o pa_extend.cmo"
OCAMLC_OPTS=-package str,core_extended,async,seq,ocaml_bio

tests=async_cmd_test

cources=global_state.ml        \
	logger.ml              \
	m_rewrite_fasta.ml     \
	async_cmd.ml           \
	copy_file.ml           \
	pm_file.ml             \
	mugsy_guide_tree.ml    \
	pm_job.ml              \
	queue_job.ml           \
	sge_interface.ml       \
	local_interface.ml     \
	script_task.ml         \
	queue_server.ml        \
	script_task_server.ml  \
	queued_task_server.ml  \
	nucmer_task.ml         \
	mugsy_task.ml          \
	mugsy_profiles_task.ml \
	fake_mugsy_task.ml     \
	job_processor.ml


sources=$(cources) paramugsy.ml

sources_mli=$(cources:%.ml=%.mli)

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
#	./queue_server_test

async_cmd_test: async_cmd_test.cmx
	ocamlfind ocamlopt -package core_extended,async -thread -linkpkg \
	-o async_cmd_test global_state.cmx async_cmd.cmx async_cmd_test.cmx

async_cmd_test.cmx: async_cmd_test.ml async_cmd.cmx global_state.cmi global_state.cmx

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
