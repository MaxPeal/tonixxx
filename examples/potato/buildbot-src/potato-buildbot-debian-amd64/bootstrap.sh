#!/bin/bash
set -euo pipefail

sudo apt-get update
sudo apt-get install -y --no-install-recommends \
    cmake \
    make \
    valgrind
sudo apt-get clean -y
sudo rm -rf /var/lib/apt/lists/*
dub fetch dscanner
