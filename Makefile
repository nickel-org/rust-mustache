all:
	rustc crate.rc

test:
	rustc --test crate.rc && ./mustache

clean:
	rm -rf *.dylib *.dSYM
