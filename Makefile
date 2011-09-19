GHCFLAGS=-Wall -fno-warn-missing-signatures -fno-warn-unused-do-bind
GHCSTRICTFLAGS=-Wall

SRC=./src/Language/Perl/Core.hs \
	./src/Language/Perl/Types.hs \
	./src/Language/Perl/Macro.hs \
	./src/Language/Perl/Parser.hs \
	./src/Language/Perl/Numeric.hs \
	./src/Language/Perl/Variables.hs \
	./src/Language/Perl/Primitives.hs \
	./src/shell.hs

all: dti

strict: $(SRC)
	ghc $(GHCSTRICTFLAGS) --make -package parsec -package ghc -o dti $(SRC)

dti: $(SRC)
	ghc $(GHCFLAGS) --make -package parsec -package ghc -o dti $(SRC)

test: dti
	./dti t.pl 

