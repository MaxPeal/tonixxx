#!/bin/bash
set -eEuo pipefail

brew update || echo 'Homebrew partially broke during update'
brew install valgrind

dub fetch dscanner
dub fetch dale
