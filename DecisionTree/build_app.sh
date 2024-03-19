stack ghc -- -Wall CART.lhs -i ../DataProcessing/DataProcessing.hs -o haskell-CART && ./haskell-CART
dot -Tx11 output/tree.dot
dot -Tpng output/tree.dot -o output/output-tree.png
