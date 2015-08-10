# Makefile

all:
	cabal sandbox init
	cabal install --only-dependencies
	cabal configure -O2
	cabal build
	cp ./dist/build/ICFP/ICFP ./play_icfp2015

