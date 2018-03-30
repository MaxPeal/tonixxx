#!/bin/sh
cabal update &&
    cabal install happy &&
    cabal install QuickCheck
