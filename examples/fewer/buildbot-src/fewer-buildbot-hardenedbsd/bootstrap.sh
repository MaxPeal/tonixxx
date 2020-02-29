#!/bin/sh
sudo hbsd-update &&
    sudo pkg update &&
    sudo pkg install -y \
        cmake \
        cppcheck \
        splint \
        valgrind \
        python3 \
        ca_root_nss &&
    sudo python3 -m ensurepip &&
    sudo pip3 install --upgrade pip &&
    sudo pip3 install cpplint &&
    sudo pkg clean -y
