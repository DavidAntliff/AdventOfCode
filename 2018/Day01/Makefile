all: test Day01 Day01b

Day01: Day01.hs Main.hs
	ghc -o $@ $^

Day01b: Day01.hs Main_b.hs
	ghc -o $@ $^

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

