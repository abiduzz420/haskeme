# Base image 
# (TODO: need to find a slimmer alternative as the base, probably alpine or debian:sid.
#        This can significantly reduce the size of image)
FROM ubuntu:18.04

# Configuring locales
ENV LANG en_US.UTF-8
ENV LC_ALL C.UTF-8

# setting up binaries/libraries
RUN apt-get update && apt-get install -y \
    curl

# fetching stack to build haskell projects
RUN curl -sSL https://get.haskellstack.org/ | sh

# updating the path and stack version
ENV PATH="/root/.local/bin:${PATH}"
RUN stack upgrade

# creating project directory
RUN mkdir -p /haskeme

# copying config files from local machine into dir
COPY stack.yaml /haskeme
COPY *.cabal /haskeme

# changing pwd
WORKDIR /haskeme

# copying all the source, test and script files (TODO: check if .dockerignore files are also being added)
COPY . .

# building the haskell project
RUN stack setup && \
    stack build --test && \
    stack install

# run the project with haskeme cli
CMD ["haskeme"]
