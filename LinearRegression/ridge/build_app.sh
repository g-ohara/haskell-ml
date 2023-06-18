#!/bin/sh

docker exec haskell-stack bash -c \
"cd ridge && stack ghc -- ridge_test1.hs -o ridge_test1 && ./ridge_test1"

docker exec -it gnuplot bash -c \
"cd ridge && gnuplot 'gnuplot.txt'"
