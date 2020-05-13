#!/bin/bash
set -eufEo pipefail

sudo apt-get update
sudo apt-get install -y \
    git \
    xz-utils \
    cmake \
    make \
    cppcheck \
    splint \
    vera++ \
    valgrind \
    python3-pip \
    python3-setuptools
sudo sh -c "echo \"export PATH=\\\"\\\$PATH:/home/vagrant/.local/bin\\\"\" >>/etc/profile"
cp -r /usr/lib/vera++ /home/vagrant/.vera++
ln -sf /vagrant/vera /home/vagrant/.vera++/profiles/default
git clone https://github.com/juj/emsdk.git
sh -c 'cd emsdk && ./emsdk install latest && ./emsdk activate latest'
sudo apt-get remove -y git xz-utils
sudo apt-get autoremove -y
sudo apt-get clean -y
sudo rm -rf /var/lib/apt/lists/* \
    /var/cache/apt/pkgcache.bin \
    /var/cache/apt/srcpkgcache.bin

pip3 install wheel
pip3 install cpplint
