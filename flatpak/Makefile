# vim: syntax=make
# Dummy Makefile to get -j value from flatpak-builder and pass it on to
# cabal install
DASH_J = $(filter -j%,$(MAKEFLAGS))
EXTRA = --extra-lib-dirs=/app/lib --extra-include-dirs=/app/include

all: build-.

# make build-foo to build subdirectory foo
# make build-. to build top-level directory
# cabal install -j does not pass -j to ghc!
#
# Write a mostly-blank config file with no remote repositories. This causes a
# warning, but without this cabal will attempt to access hackage.haskell.org
# and fail because there is no network access.
build-%:
	mkdir -p $$HOME/.cabal
	echo 'jobs: $$ncpus' > $$HOME/.cabal/config

	( cd $* && cabal configure --global $(EXTRA) )
	( cd $* && cabal build $(DASH_J) )
	( cd $* && cabal copy )
	( cd $* && cabal register )

install:

# Don't run rules within this file in parallel
.NOTPARALLEL:
