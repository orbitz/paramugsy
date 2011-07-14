OCAMLMAKEFILE = ../OCamlMakefile

USE_CAMLP4 = yes

INCDIRS = +camlp4

THREADS = yes

SOURCES = global_state.ml \
	m_rewrite_fasta.mli m_rewrite_fasta.ml \
	pm_sge_utils.ml \
	mugsy_guide_tree.ml \
	pm_sge.ml \
	pm_ergatis.ml \
	paramugsy.ml

RESULT  = paramugsy

PACKS = str core_extended lwt lwt.unix lwt.syntax seq ocaml_bio

-include $(OCAMLMAKEFILE)
