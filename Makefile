RUSTC ?= rustc
RUST_FLAGS ?= -O

.PHONY: install clean test

all: build
	$(RUSTC) $(RUST_FLAGS) --out-dir=build src/mustache/lib.rs

build:
	test -d build || mkdir build

test: build
	$(RUSTC) --test src/mustache/lib.rs -o build/test -L build && ./build/test

clean:
	rm -rf bin/* build/* lib/*
