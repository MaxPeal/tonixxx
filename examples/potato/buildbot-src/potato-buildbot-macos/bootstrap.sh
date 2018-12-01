#!/bin/bash
set -euo pipefail

brew install findutils --with-default-names
brew install \
    cmake \
    valgrind
dub fetch dscanner
