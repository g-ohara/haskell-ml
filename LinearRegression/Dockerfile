# first, I described 'FROM haskell'.
# but when installing hmatrix package, I need to install some packages via apt.
# then I changed into 'FROM ubuntu'.
FROM ubuntu:latest

# install stack
RUN apt update && apt install -y haskell-stack && stack update && \
    stack upgrade --binary-only && stack setup --install-ghc

# install hmatrix (Numeric.LinearAlgebra)
RUN apt update && apt install -y liblapack-dev liblapack-doc && \
    stack install hmatrix

# install gnuplot
RUN apt update && apt install -y gnuplot gnuplot-x11

# install x11 applications to run xeyes
RUN apt update && apt install -y x11-apps
