#!/bin/sh
su root -c "pkgin update && pkgin -y install gmake" &&
    rustup update nightly &&
    rustup default nightly
