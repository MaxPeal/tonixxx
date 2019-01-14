#!/bin/bash
set -eEuo pipefail

brew update || echo 'Homebrew partially broke during update'
brew install findutils --with-default-names
brew install \
    cmake \
    cppcheck \
    splint \
    vera++ \
    valgrind \
    python3
cp -r /usr/local/lib/vera++ /Users/vagrant/.vera++
ln -sf /vagrant/vera /Users/vagrant/.vera++/profiles/default

pip3 install wheel
pip3 install cpplint
