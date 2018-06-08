#!/bin/sh
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" &&
    brew install \
    mint \
    infer &&
    mint install realm/SwiftLint
