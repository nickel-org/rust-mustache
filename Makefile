RUSTC ?= rustc
RUSTPKG ?= rustpkg

all:
	$(RUSTPKG) install mustache

check:
	$(RUSTC) --test -o bin/test-mustache src/mustache/lib.rs
	./bin/test-mustache

clean:
	rm -rf bin build lib
