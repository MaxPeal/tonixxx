module Main where

import Data.Monoid

import Test.Framework.Runners.Console
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import MoTest

import qualified Mo

main :: IO ()
main = defaultMainWithOpts [
  testCase "zeroEncoded" testZeroEncoded,
  testCase "zeroDecoded" testZeroDecoded,
  testCase "oneEncoded" testOneEncoded,
  testCase "oneDecoded" testOneDecoded,
  testCase "twoEncoded" testTwoEncoded,
  testCase "twoDecoded" testTwoDecoded,
  testProperty "reversible" Mo.propReversible
  ] mempty
