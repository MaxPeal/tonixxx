#!/bin/sh
sudo pkg update &&
    sudo pkg install -y \
        cmake \
        valgrind &&
    sudo pkg clean -y &&
    dub fetch dscanner
