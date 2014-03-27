RUSTC ?= rustc
RUST_FLAGS ?= -O --crate-type=rlib,dylib


.PHONY: install clean test

all:
	test -d build || mkdir build
	$(RUSTC) $(RUST_FLAGS) --out-dir=build src/mustache/mustache.rs

test: 
	test -d build || mkdir build
	$(RUSTC) --test src/mustache/test.rs -o build/libtest~ -L build && ./build/libtest~

clean:
	rm -rf bin/* build/* lib/*
