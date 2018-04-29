#!/bin/sh
sudo apt-get update &&
    sudo apt-get install -y \
        build-essential \
        cmake \
        cppcheck \
        splint \
        vera++
