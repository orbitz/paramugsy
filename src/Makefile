SUBDIRS = base mugsy nucmer profiles profiles_lib m_translate

EXTERNAL_APPS = ocaml-bio ocaml-seq

all_apps = $(SUBDIRS) $(EXTERNAL_APPS)

subdirs_clean = $(all_apps:%=%_clean)

subdirs_test  = $(all_apps:%=%_test)

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

.PHONY: all test clean $(SUBDIRS) $(EXTERNAL_APPS) $(subdirs_clean) \
	seq ocaml_bio seq_lcean ocaml_bio_clean

all: $(SUBDIRS)

test: $(subdirs_test)

clean: $(subdirs_clean) ocaml_bio_clean seq_clean

base: ocaml_bio/META seq/META

mugsy: ocaml_bio/META seq/META

nucmer: seq/META

profiles: ocaml_bio/META seq/META

m_translate: profiles_lib

$(SUBDIRS):
	$(MAKE) -C $@ native-code

$(subdirs_test):
	$(MAKE) -C $(patsubst %_test,%,$@) test

$(subdirs_clean):
	$(MAKE) -C $(patsubst %_clean,%,$@) clean

ocaml-seq:
	$(MAKE) -C ocaml-seq

ocaml-bio: seq/META
	$(MAKE) -C ocaml-bio

# Depend on the META begin there so we don't
# try to install every time
ocaml_bio/META: ocaml-bio
	-cd ocaml-bio/lib && \
	ocamlfind install -destdir ../../ ocaml_bio META $(OCAML_BIO_INSTALL_FILES)

ocaml_bio_clean:
	-rm -rf ocaml_bio

seq/META: ocaml-seq
	-cd ocaml-seq/lib && \
	ocamlfind install -destdir ../../ seq META $(SEQ_INSTALL_FILES)

seq_clean:
	-rm -rf seq
