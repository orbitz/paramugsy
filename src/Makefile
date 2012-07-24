SUBDIRS = base mugsy nucmer profiles profiles_lib m_translate

subdirs_native = $(SUBDIRS:%=%_native)
subdirs_byte   = $(SUBDIRS:%=%_byte)
subdirs_debug  = $(SUBDIRS:%=%_debug)
subdirs_clean  = $(SUBDIRs:%=%_clean)

.PHONY: native byte debug clean $(subdirs_native) $(subdirs_byte) $(subdirs_debug) $(subdirs_clean)

native: $(subdirs_native)

byte: $(subdirs_byte)

debug: $(subdirs_debug)

clean: $(subdirs_clean)

m_translate: profiles_lib

$(subdirs_native):
	$(MAKE) -C $(patsubst %_native,%,$@) native-code

$(subdirs_byte):
	$(MAKE) -C $(patsubst %_byte,%,$@) byte-code

$(subdirs_debug):
	$(MAKE) -C $(patsubst %_debug,%,$@) debug-code

$(subdirs_clean):
	$(MAKE) -C $(patsubst %_clean,%,$@) clean
