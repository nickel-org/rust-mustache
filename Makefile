all:
	rustc mustache.rc

test:
	rustc --test mustache.rc

clean:
	rm -rf *.dylib *.dSYM
