# compile and test with cabal

``` bash
cabal test
```

# code format

Bobek uses ormolu for code formatting.  Make sure that it is already installed.
``` bash
cabal new-install ormolu
```

``` bash
make format
```

# coverage information

``` bash
cabal clean
cabal new-configure --enable-test --enable-coverage
caba new-test
```

# linter

Bobek uses HLint for code linting.  Make sure that it is already installed.
``` bash
make lint
```
