#!/bin/sh
sudo apt-get update &&
    sudo apt-get install -y \
        make \
        infer \
        git &&
    git clone https://github.com/yonaskolb/Mint.git &&
    sh -c 'cd Mint && make && sudo make install' &&
    mint install realm/SwiftLint
