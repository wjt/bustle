# vim: syntax=make
EXTRA = --extra-lib-dirs=$(LD_LIBRARY_PATH) --extra-include-dirs=$(C_INCLUDE_PATH)

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
	echo 'jobs: $(FLATPAK_BUILDER_N_JOBS)' > $$HOME/.cabal/config

	( cd $* && cabal configure --global $(EXTRA) )
	( cd $* && cabal build -j$(FLATPAK_BUILDER_N_JOBS) )
	( cd $* && cabal copy )
	( cd $* && cabal register )

install:

# Don't run rules within this file in parallel
.NOTPARALLEL:
