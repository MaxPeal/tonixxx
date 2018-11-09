#!/bin/sh
sudo xbps-install -uSy \
    make \
    cmake \
    cppcheck \
    splint \
    valgrind &&
    sudo xbps-remove -O
