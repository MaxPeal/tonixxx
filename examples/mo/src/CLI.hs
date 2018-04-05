module Main where

import qualified Numeric.Natural as NN
import qualified System.Environment as SE
import qualified Test.QuickCheck as QC

import qualified Mo

main :: IO ()
main = do
  args <- SE.getArgs

  if args == ["-t"]
    then QC.quickCheck Mo.propReversible
    else interact (unlines . map (show . Mo.Unary . fromInteger . read) . lines)
