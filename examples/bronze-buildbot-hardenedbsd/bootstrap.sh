#!/bin/sh
sudo pkg install -y gmake &&
    rustup update nightly &&
    rustup default nightly
