#!/bin/sh
sudo pkg update &&
    sudo pkg install -y \
        cmake \
        cppcheck \
        splint \
        vera++ \
        python3 \
        py36-pip &&
    sudo pkg clean -y &&
    sudo pip-3.6 install wheel &&
    sudo pip-3.6 install cpplint
