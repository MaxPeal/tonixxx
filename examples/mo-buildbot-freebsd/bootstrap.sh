#!/bin/sh
cabal update &&
    cabal install happy &&
    cabal install HLint &&
    cabal install shake
