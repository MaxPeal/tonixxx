#!/bin/bash
set -euo pipefail

sudo apt-get update
sudo apt-get install -y --no-install-recommends \
    cmake \
    valgrind
sudo apt-get clean -y
rm -rf /var/lib/apt/lists/*
dub fetch dscanner
