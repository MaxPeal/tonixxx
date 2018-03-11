#!/bin/sh
sudo pkg update &&
    sudo pkg install -y \
        devel/readline \
        cmake \
        cppcheck \
        splint \
        vera++ &&
    sudo pkg clean -y
