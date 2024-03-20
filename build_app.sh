stack ghc -- -Wall haskell-ml.lhs -i DataProcessing/DataProcessing.hs -o haskell-ml && ./haskell-ml
dot -Tx11 output/tree.dot
dot -Tpng output/tree.dot -o output/output-tree.png
