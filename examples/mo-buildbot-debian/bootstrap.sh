#!/bin/sh
sudo apt-get update &&
    sudo apt-get install -y make &&
    cabal update &&
    cabal install happy &&
    cabal install hlint &&
    cabal install QuickCheck
