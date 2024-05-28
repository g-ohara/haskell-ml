# Haskell-ML
This repository contains PDF book that includes Haskell code for machine learning.
I write this book to learn Haskell and machine learning and hope that it will be helpful for those who have the same interest.

## How to compile it
The haskell-ml.pdf is generated from haskell-ml.lhs, written in [Literate Haskell](https://wiki.haskell.org/Literate_programming) format.
You can compile it as both Haskell (.hs) and TeX (.tex) source code.

### Prerequisites
You need to have installed the following apps on your computer:
- [Docker](https://www.docker.com/)
- [Docker Compose](https://docs.docker.com/compose/) (Optional)

### Compile as Haskell code
If you have Docker Compose installed on your computer, run:
```sh
docker compose run --rm hls cabal run
```
Or if not, run:
```sh
docker run --rm -v ./:$PWD -w $PWD genjiohara/haskell-language-server cabal run
```

### Compile as TeX code
If you have Docker Compose installed on your computer, run:
```sh
docker compose run --rm texlab latexmk
```
Or if not, run:
```sh
docker run --rm -v ./:$PWD -w $PWD genjiohara/texlab latexmk
```
