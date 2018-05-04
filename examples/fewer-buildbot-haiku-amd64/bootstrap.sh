#!/bin/sh
pkgman refresh &&
    pkgman install -y \
        cmake \
        cppcheck
