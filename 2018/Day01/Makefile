all: test Day01Part1 Day01Part2

Day01Part1: Day01.hs MainPart1.hs
	ghc -o $@ $^

Day01Part2: Day01.hs MainPart2.hs
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

