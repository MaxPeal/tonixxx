#!/bin/bash
set -eEuo pipefail

apt-get update
apt-get install -y \
    cmake \
    make \
    cppcheck \
    splint \
    vera++ \
    valgrind \
    python3-pip \
    python3-setuptools
echo "export PATH=\"\$PATH:/home/vagrant/.local/bin\"" >>/etc/profile
cp -r /usr/lib/vera++ /home/vagrant/.vera++
ln -sf /vagrant/vera /home/vagrant/.vera++/profiles/default
apt-get clean -y
rm -rf /var/lib/apt/lists/* \
    /var/cache/apt/pkgcache.bin \
    /var/cache/apt/srcpkgcache.bin

pip3 install wheel
pip3 install cpplint
