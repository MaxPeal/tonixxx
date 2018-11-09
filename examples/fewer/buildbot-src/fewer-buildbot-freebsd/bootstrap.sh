#!/bin/sh
sudo pkg update &&
    sudo pkg install -y \
        cmake \
        cppcheck \
        splint \
        vera++ \
        valgrind &&
    sudo pkg clean -y
