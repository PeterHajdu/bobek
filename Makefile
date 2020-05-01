format:
	ormolu -m inplace `find src lib test -iname "*.hs"`

lint:
	hlint -g

.PHONY: format lint
