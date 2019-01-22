.PHONY: build clean

build:
	dune build @install
	ln -sf _build/install/default/bin .

clean:
	dune clean
	rm -f bin doc
