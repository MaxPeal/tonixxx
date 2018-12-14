#!/bin/sh
sudo pkg update &&
    sudo pkg install -y \
        meson \
        valgrind &&
    sudo pkg clean -y &&
    dub fetch dscanner
