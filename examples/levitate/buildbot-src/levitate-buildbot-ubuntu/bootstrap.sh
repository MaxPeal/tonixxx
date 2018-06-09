#!/bin/sh
sudo apt-get update &&
    sudo apt-get install -y make git &&
    git clone https://github.com/realm/SwiftLint.git &&
    sh -c 'cd SwiftLint && git submodule update --init --recursive && sudo make install'
