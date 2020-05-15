format:
	ormolu -m inplace `find src lib test -iname "*.hs"`

lint:
	hlint -g

test:
	cabal clean
	cabal new-configure --enable-test --enable-coverage
	cabal new-test

.PHONY: format lint test
