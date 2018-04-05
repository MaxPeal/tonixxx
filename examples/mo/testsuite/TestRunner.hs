module Main where

import MoTest

import Test.Framework.Runners.Console
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

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
