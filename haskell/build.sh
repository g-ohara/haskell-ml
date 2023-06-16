docker exec DT-stack bash -c "stack ghc -- CART.lhs -o haskell-CART && ./haskell-CART" && \
docker exec DT-graphviz dot -Tx11 tree.dot
docker exec DT-graphviz dot -Tpng tree.dot -o output-tree.png