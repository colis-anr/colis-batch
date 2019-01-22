.PHONY: build clean install uninstall

build:
	dune build @install
	ln -sf _build/install/default/bin .

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f bin doc
