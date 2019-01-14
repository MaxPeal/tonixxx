#!/bin/sh
sudo yum update -y &&
    sudo yum install -y https://centos7.iuscommunity.org/ius-release.rpm &&
    sudo yum install -y epel-release &&
    sudo yum install -y \
        cmake3 \
        cppcheck \
        splint \
        valgrind \
        python36u \
        python36u-pip &&
    sudo pip3.6 install cpplint
