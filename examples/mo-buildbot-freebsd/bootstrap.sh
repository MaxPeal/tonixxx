#!/bin/sh
cabal update &&
    cabal install happy &&
    cabal install hlint &&
    cabal install QuickCheck
