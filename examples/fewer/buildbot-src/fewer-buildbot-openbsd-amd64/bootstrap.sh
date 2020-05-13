#!/bin/ksh
set -euf

sudo pkg_add -u
sudo pkg_add \
    cmake \
    cppcheck \
    splint \
    valgrind \
    python-3.6.4p0 \
    py3-pip

sudo pip3.6 install cpplint
