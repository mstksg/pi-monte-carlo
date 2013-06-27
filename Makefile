HS_SRC=$(wildcard src/*.hs src/*/*.hs)

.PHONY: install test

install: bin/pimc

test: bin/pimc
	bin/pimc

bin/pimc: $(HS_SRC)
	cabal configure
	cabal build
	ln -fs dist/build/pimc/pimc bin/pimc

