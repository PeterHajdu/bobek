format:
	ormolu -m inplace `find src lib test -iname "*.hs"`

.PHONY: format
