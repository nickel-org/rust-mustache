RUSTC ?= rustc
RUSTDOC ?= rustdoc
RUST_FLAGS ?= -O
LIB = build/mustache.libstamp

.PHONY: install clean test

all: $(LIB)

$(LIB): src/mustache/lib.rs
	test -d build || mkdir build
	$(RUSTC) $(RUST_FLAGS) --out-dir=build src/mustache/lib.rs && touch $@

build:
	mkdir build

test: $(LIB)
	$(RUSTC) --test src/mustache/lib.rs -o build/test -L build && ./build/test

checkdocs: $(LIB)
	$(RUSTDOC) --test README.md -L build

clean:
	rm -rf build/
