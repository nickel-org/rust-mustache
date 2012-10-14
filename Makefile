RUSTC ?= rustc

dummy1 := $(shell mkdir bin 2> /dev/null)

all:
	$(RUSTC) --lib -o bin/mustache mustache.rc

check:
	$(RUSTC) --test -o bin/test-mustache mustache.rc && ./bin/test-mustache

check1:
	$(RUSTC) --test -o bin/test-mustache mustache.rc && ./bin/test-mustache test_spec_interpolation

clean:
	rm -rf bin
