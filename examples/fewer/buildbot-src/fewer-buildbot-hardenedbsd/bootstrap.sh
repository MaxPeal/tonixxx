#!/bin/sh
sudo hbsd-update &&
    sudo pkg update &&
    sudo pkg install -y \
        cmake \
        splint \
        vera++ \
        valgrind &&
    sudo pkg clean -y
