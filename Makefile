.PHONY: all
all:
	@cd src_base && $(MAKE) byte-code 
	@cd src_profiles && $(MAKE) byte-code
	@cd src_nucmer && $(MAKE) byte-code
	@cd src_mugsy && $(MAKE) byte-code
	@cd src_profiles_cpp && $(MAKE)

.PHONY: native
native:
	@cd src_base && $(MAKE) native-code 
	@cd src_profiles && $(MAKE) native-code
	@cd src_nucmer && $(MAKE) native-code
	@cd src_mugsy && $(MAKE) native-code
	@cd src_profiles_cpp && $(MAKE)


.PHONY: debug
debug:
	@cd src_base && $(MAKE) debug-code
	@cd src_profiles && $(MAKE) debug-code
	@cd src_nucmer && $(MAKE) debug-code
	@cd src_mugsy && $(MAKE) debug-code
	@cd src_profiles_cpp && $(MAKE)


.PHONY:	clean
clean:
	@cd src_base && $(MAKE) clean
	@cd src_profiles && $(MAKE) clean
	@cd src_nucmer && $(MAKE) clean
	@cd src_mugsy && $(MAKE) clean
	@cd src_profiles_cpp && $(MAKE) clean

