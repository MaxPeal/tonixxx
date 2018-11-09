#!/bin/sh
sudo xbps-install -uSy \
    musl \
    musl-devel &&
    sudo xbps-install -uSy \
        make \
        cmake \
        cppcheck \
        splint \
        valgrind \
        python3-pip &&
    sudo xbps-remove -O;
    sudo pip3 install wheel &&
    sudo pip3 install cpplint &&
    sudo sh -c 'echo "export PATH=\"\$PATH:/home/vagrant/.local/bin\"" >>/etc/profile'
