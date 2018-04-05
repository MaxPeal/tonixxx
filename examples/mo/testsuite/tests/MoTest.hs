module MoTest (
  testZeroEncoded,
  testZeroDecoded,
  testOneEncoded,
  testOneDecoded,
  testTwoEncoded,
  testTwoDecoded
  ) where

import qualified Mo

import Test.HUnit

testZeroEncoded :: Assertion
testZeroEncoded = (read :: String -> Mo.Unary) "" @?= Mo.Unary 0

testZeroDecoded :: Assertion
testZeroDecoded = show (Mo.Unary 0) @?= ""

testOneEncoded :: Assertion
testOneEncoded = (read :: String -> Mo.Unary) "1" @?= Mo.Unary 1

testOneDecoded :: Assertion
testOneDecoded = show (Mo.Unary 1) @?= "1"

testTwoEncoded :: Assertion
testTwoEncoded = (read :: String -> Mo.Unary) "11" @?= Mo.Unary 2

testTwoDecoded :: Assertion
testTwoDecoded = show (Mo.Unary 2) @?= "11"
