docker exec haskell-DNN-stack bash -c \
"stack ghc -- two_layer_net.lhs -o two_layer_net && ./two_layer_net"