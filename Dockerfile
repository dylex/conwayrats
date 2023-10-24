FROM fpco/stack-build-small:lts-19.33 AS build
RUN apt-get update && \
    apt-get install -y libglpk-dev
WORKDIR /tmp/conwayrats
COPY stack.yaml *.cabal ./
RUN stack build --system-ghc --dependencies-only
COPY *.hs case4 README.md ./
RUN stack install --system-ghc && rm -rf .stack-work
ENTRYPOINT ["/root/.local/bin/rats"]

#FROM ubuntu:18.04
#COPY --from=build /root/.local/bin/rats /usr/local/bin/rats
#ENTRYPOINT ["/usr/local/bin/rats"]

