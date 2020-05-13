#!/bin/bash
set -eufEo pipefail

brew update || echo 'Homebrew partially broke during update'
brew install \
    cmake \
    cppcheck \
    splint \
    vera++ \
    valgrind
cp -r /usr/local/lib/vera++ /Users/vagrant/.vera++
ln -sf /vagrant/vera /Users/vagrant/.vera++/profiles/default

pip3 install cpplint
