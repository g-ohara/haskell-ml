# Haskell-ML
This repository contains PDF book that includes Haskell code for machine learning.
I write this book to learn Haskell and machine learning and hope that it will be helpful for those who have the same interest.

## How to compile it
The haskell-ml.pdf is generated from haskell-ml.lhs, written in [Literate Haskell](https://wiki.haskell.org/Literate_programming) format.
You can compile it as both Haskell (.hs) and TeX (.tex) source code.

### Compile as Haskell code

#### Requirements
- [ghc](https://www.haskell.org/ghc/)
- [hmatrix](https://hackage.haskell.org/package/hmatrix)
- [MissingH](https://hackage.haskell.org/package/MissingH)
```shell
# Install ghc 
$ sudo apt update && sudo apt install -y haskell-stack g++
$ stack update && stack upgrade --binary-only && stack setup --install-ghc
```
```shell
# Install hmatrix (Numeric.LinearAlgebra) and MissingH (Data.CSV)
$ sudo apt update && sudo apt install -y liblapack-dev liblapack-doc
$ stack update & stack install hmatrix MissingH
```

#### Compile and Run
```shell
$ ./build_app.sh
```

### Compile as TeX code
#### Requirements
- [TeX Live](https://www.tug.org/texlive/) (full version)
```shell
# Install TeX Live
$ sudo apt update && sudo apt install -y texlive-full
```

#### Compile
```shell
$ ./build_doc.sh
```
