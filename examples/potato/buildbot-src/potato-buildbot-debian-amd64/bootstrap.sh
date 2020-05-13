#!/bin/bash
set -eufEo pipefail

sudo apt-get update

sudo apt-get install -y --no-install-recommends valgrind

sudo apt-get clean -y

sudo rm -rf /var/lib/apt/lists/*

dub fetch dscanner

dub fetch dale
