RUSTC ?= rustc
RUSTPKG ?= rustpkg
RUST_FLAGS ?= -Z debug-info -O

all:
	$(RUSTPKG) $(RUST_FLAGS) install mustache

check:
	mkdir -p bin
	$(RUSTC) $(RUST_FLAGS) --test -o bin/test-mustache src/mustache/lib.rs
	./bin/test-mustache

clean:
	rm -rf bin build lib
