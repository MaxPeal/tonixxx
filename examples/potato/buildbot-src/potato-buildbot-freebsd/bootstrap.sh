#!/bin/sh
sudo pkg update &&
    sudo pkg install -y valgrind &&
    sudo pkg clean -y &&
    dub fetch dscanner &&
    dub fetch dale
