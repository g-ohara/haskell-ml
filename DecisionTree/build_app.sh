docker exec DT-stack bash -c \
    "stack ghc -- -Wall CART.lhs -i ../DataProcessing/DataProcessing.hs -o haskell-CART && ./haskell-CART" && \
    xhost +local:docker
docker exec DT-graphviz dot -Tx11 output/tree.dot
xhost -local:docker
docker exec DT-graphviz dot -Tpng output/tree.dot -o output/output-tree.png
