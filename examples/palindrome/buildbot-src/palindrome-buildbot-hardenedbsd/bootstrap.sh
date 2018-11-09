#!/bin/sh
sudo hbsd-update &&
    sudo pkg update &&
    sudo pkg install -y \
        cmake \
        splint \
        vera++ \
        valgrind \
        python3 \
        ca_root_nss &&
    sudo pkg clean -y &&
    sudo python3 -m ensurepip &&
    sudo pip3 install cpplint
