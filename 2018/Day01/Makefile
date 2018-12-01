all: test

test: format
	cabal test

build: format
	cabal build

format:
	#DANGEROUS./hindent-all

clean:
	cabal clean

setup:
	cabal update
	cabal install hindent
	cabal install --run-tests

