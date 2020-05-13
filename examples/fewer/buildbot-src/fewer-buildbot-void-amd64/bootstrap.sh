#!/bin/bash
set -eufEo pipefail

sudo xbps-install -uSy \
    make \
    cmake \
    cppcheck \
    splint \
    valgrind \
    python3 \
    python3-pip
sudo xbps-remove -O || echo 'Cleared package cache'

sudo pip3 install wheel
sudo pip3 install cpplint
