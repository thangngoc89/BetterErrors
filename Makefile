build:
	# Creating ocamlBetterErrors.opam so that jbuilder builds.
	jbuilder build -j 8

install: build 
	esy-installer refmterr
	esy-installer refmterr-lib

test:
	jbuilder runtest

clean:
	rm *.opam
	jbuilder clean

.PHONY: build release test
