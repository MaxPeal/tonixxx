#!/bin/sh
sudo apt-get update &&
    sudo apt-get install -y \
        build-essential \
        libreadline-dev \
        cmake \
        cppcheck \
        splint \
        vera++