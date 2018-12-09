DAY:=04
PARTS:=1 #2

TARGETS:=$(addprefix Day$(DAY)Part, $(PARTS))

# Add any local modules required here
MODULES:= Record.hs

all: test $(TARGETS)

$(TARGETS): Day$(DAY)Part%: $(MODULES) MainPart%.hs Day$(DAY).hs
	ghc -o $@ $^

test: #format
	cabal test

build: #format
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

