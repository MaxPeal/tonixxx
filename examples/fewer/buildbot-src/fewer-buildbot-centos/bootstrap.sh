#!/bin/sh
sudo yum update -y &&
    sudo yum install -y epel-release
    sudo yum install -y \
        cmake3 \
        cppcheck \
        splint
