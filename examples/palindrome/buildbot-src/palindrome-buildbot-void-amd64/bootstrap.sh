#!/bin/bash
set -eEuo pipefail

sudo xbps-install -uSy \
    musl \
    musl-devel
sudo xbps-install -uSy \
    make \
    cmake \
    cppcheck \
    valgrind \
    python3-pip
sudo sh -c 'echo "export PATH=\"\$PATH:/home/vagrant/.local/bin\"" >>/etc/profile'
sudo xbps-remove -O || echo 'Cleared package cache'

sudo pip3 install wheel
sudo pip3 install cpplint
