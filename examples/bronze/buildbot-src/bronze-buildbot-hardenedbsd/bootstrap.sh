#!/bin/sh
sudo hbsd-update &&
    sudo pkg update &&
    sudo pkg install -y gmake &&
    rustup update nightly &&
    rustup default nightly
