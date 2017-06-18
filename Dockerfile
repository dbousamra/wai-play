FROM haskell:8.0.2
MAINTAINER Dominic Bou-Samra <dom@imageintelligence.com>

COPY . /root/app
WORKDIR /root/app

RUN cabal update

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal install --only-dependencies -j4

# RUN stack build


CMD ["echo DOM"]

