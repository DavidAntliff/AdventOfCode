DAY:=03
PARTS:=1 #2

TARGETS:=$(addprefix Day$(DAY)Part, $(PARTS))

all: test $(TARGETS)

$(TARGETS): Day$(DAY)Part%: Claim.hs MainPart%.hs Day$(DAY).hs
	ghc -o $@ $^

#Day01Part1: Day01.hs MainPart1.hs
#	ghc -o $@ $^

#Day01Part2: Day01.hs MainPart2.hs
#	ghc -o $@ $^

test: format
	cabal test

build: format
	cabal build

format:
	#DANGEROUS./hindent-all

clean:
	cabal clean
	rm -f *.o *.hi
	rm -f $(TARGETS)

setup:
	cabal update
	cabal install hindent
	cabal install --run-tests

