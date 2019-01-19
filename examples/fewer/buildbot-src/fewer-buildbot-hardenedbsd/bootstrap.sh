#!/bin/sh
sudo hbsd-update &&
    sudo pkg update &&
    sudo pkg install -y \
        cmake \
        cppcheck \
        splint \
        vera++ \
        valgrind \
        python3 \
        ca_root_nss &&
    cp -r /usr/local/lib/vera++ /home/vagrant/.vera++ &&
    ln -sf /vagrant/vera /home/vagrant/.vera++/profiles/default &&
    sudo python3 -m ensurepip &&
    sudo pip3 install --upgrade pip &&
    sudo pip3 install cpplint &&
    sudo pkg clean -y
