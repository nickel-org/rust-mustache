RUSTC ?= rustc

dummy1 := $(shell mkdir bin 2> /dev/null)

all:
	$(RUSTC) --lib -o bin/mustache mustache.rs

check:
	$(RUSTC) --test -o bin/test-mustache mustache.rs && ./bin/test-mustache

clean:
	rm -rf bin
