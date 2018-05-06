#!/bin/sh
brew install \
    cmake \
    cppcheck \
    splint \
    vera++ \
    python3 &&
    pip3 install wheel &&
    pip3 install cpplint
