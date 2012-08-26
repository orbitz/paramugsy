SUBDIRS = base mugsy nucmer profiles profiles_lib m_translate ocaml-bio ocaml-seq

subdirs_clean = $(SUBDIRS:%=%_clean)

OCAML_BIO_INSTALL_FILES = bio_string_ext.mli bio_string_ext.cmi \
	bio_fasta.mli bio_fasta.cmi \
	bio_genbank.mli bio_genbank.cmi \
	bio_csv.mli bio_csv.cmi \
	bio_newick.mli bio_newick.cmi \
	bio.mli bio.cmi \
	ocaml_bio.cma ocaml_bio.cmxa ocaml_bio.a

SEQ_INSTALL_FILES = ort_seq.mli ort_seq.cmi \
	ort_fileutils.mli ort_fileutils.cmi \
	ort_lazy_io.mli ort_lazy_io.cmi \
	ort_function.mli ort_function.cmi \
	ort.mli ort.cmi \
	seq.cma seq.cmxa seq.a

.PHONY: all clean $(SUBDIRS) $(subdirs_clean) \

all: $(SUBDIRS)

clean: $(subdirs_clean) ocaml_bio_clean seq_clean

base: ocaml_bio seq

mugsy: ocaml_bio seq

nucmer: seq

profiles: ocaml_bio seq

m_translate: profiles_lib

$(SUBDIRS):
	$(MAKE) -C $@

$(subdirs_clean):
	$(MAKE) -C $(patsubst %_clean,%,$@) clean


# Depend on the META begin there so we don't
# try to install every time
ocaml_bio: ocaml-bio ocaml_bio/META

ocaml_bio/META:
	cd ocaml-bio/lib && \
	ocamlfind install -destdir ../../ ocaml_bio META $(OCAML_BIO_INSTALL_FILES)

ocaml_bio_clean:
	-rm -rf ocaml_bio

seq: ocaml-seq seq/META

seq/META:
	cd ocaml-seq/lib && \
	ocamlfind install -destdir ../../ seq META $(SEQ_INSTALL_FILES)

seq_clean:
	-rm -rf seq
