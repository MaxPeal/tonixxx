#!/bin/sh
sudo yum update -y &&
    sudo yum install -y patch &&
    cabal update &&
    cabal install cabal-install-1.18.2.0 &&
    sh -c 'cd /etc/profile.d && sudo patch </home/vagrant/cabal-install.sh.patch' &&
    . /home/vagrant/.bash_profile &&
    cabal update &&
    cabal install happy &&
    cabal install hlint &&
    cabal install shake
