#!/bin/sh
sudo apt-get update &&
    sudo apt-get install -y \
        build-essential \
        cmake \
        cppcheck \
        splint \
        vera++ \
        valgrind &&
    sudo apt-get clean -y &&
    sudo rm -rf /var/lib/apt/lists/*
