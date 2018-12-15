#!/bin/bash
set -eEuo pipefail

brew install valgrind

dub fetch dscanner

dub fetch dale
