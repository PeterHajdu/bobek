format:
	ormolu -m inplace `find src test -iname "*.hs"`

.PHONY: format
