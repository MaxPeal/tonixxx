#!/bin/sh
sudo xbps-install -uSy \
    make \
    cmake \
    cppcheck \
    splint &&
    sudo xbps-remove -O
