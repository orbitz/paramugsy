.PHONY: all native byte debug clean

all: native

native:
	$(MAKE) -C src native

byte:
	$(MAKE) -C src byte

debug:
	$(MAKE) -C src debug

clean:
	$(MAKE) -C src clean

