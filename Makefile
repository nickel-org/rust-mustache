all:
	rustc mustache.rc

test:
	rustc --test mustache.rc && ./mustache

clean:
	rm -rf *.dylib *.dSYM
