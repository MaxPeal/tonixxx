#!/bin/sh
sudo xbps-install -uSy \
    make \
    cmake \
    cppcheck \
    splint \
    python3-pip &&
    sudo xbps-remove -O;
    sudo pip3 install wheel &&
    sudo pip3 install cpplint &&
    sudo sh -c 'echo "export PATH=\"\$PATH:/home/vagrant/.local/bin\"" >>/etc/profile'
