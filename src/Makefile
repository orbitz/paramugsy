SUBDIRS = base mugsy nucmer profiles profiles_lib m_translate ocaml-bio ocaml-seq

subdirs_native = $(SUBDIRS:%=%_native)
subdirs_byte   = $(SUBDIRS:%=%_byte)
subdirs_debug  = $(SUBDIRS:%=%_debug)
subdirs_clean  = $(SUBDIRS:%=%_clean)

.PHONY: all native byte debug clean \
	$(SUBDIRS) $(subdirs_native) $(subdirs_byte) \
	$(subdirs_debug) $(subdirs_clean)

all: native

native: $(subdirs_native)

byte: $(subdirs_byte)

debug: $(subdirs_debug)

clean: $(subdirs_clean)

base: ocaml-bio ocaml-seq

mugsy: ocaml-bio ocaml-seq

nucmer: ocaml-seq

profiles: ocaml-bio ocaml-seq

m_translate: profiles_lib

$(subdirs_native):
	$(MAKE) -C $(patsubst %_native,%,$@) native-code

$(subdirs_byte):
	$(MAKE) -C $(patsubst %_byte,%,$@) byte-code

$(subdirs_debug):
	$(MAKE) -C $(patsubst %_debug,%,$@) debug-code

$(subdirs_clean):
	$(MAKE) -C $(patsubst %_clean,%,$@) clean
