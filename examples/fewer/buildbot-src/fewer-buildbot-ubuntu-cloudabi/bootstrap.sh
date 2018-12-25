#!/bin/bash
set -eEuo pipefail

apt-get update

apt-get install -y \
    cmake \
    cppcheck \
    splint \
    vera++ \
    valgrind

apt-get clean -y

rm -rf /var/lib/apt/lists/*
