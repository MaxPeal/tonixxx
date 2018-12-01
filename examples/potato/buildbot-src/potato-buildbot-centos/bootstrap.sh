#!/bin/bash
set -euo pipefail

sudo yum update -y
sudo yum install -y epel-release
sudo yum install -y \
    cmake3 \
    valgrind
dub fetch dscanner
