module Main where

import qualified Numeric.Natural as NN
import qualified Data.Int as DI
import qualified Test.QuickCheck as QC
import qualified System.Environment as SE

data Unary = Unary NN.Natural deriving (Eq, Ord)

nat :: Unary -> NN.Natural
nat (Unary x) = x

instance Show Unary where
  show (Unary 0) = ""
  show (Unary v) = "1" ++ show (Unary (v - 1))

instance Read Unary where
  readsPrec _ "" = [(Unary 0, "")]
  readsPrec _ ('1' : ones) = [(Unary (1 + nat (readUnary ones)), "")]
  readsPrec _ _ = error "Unary values consist of a non-negative series of '1's"

propReversible :: DI.Int8 -> Bool
propReversible x
  | x < 0 = True
  | otherwise = u == u' where
    u = (Unary . fromInteger . toInteger) x
    u' = (readUnary . show) u

readInteger :: String -> Integer
readInteger = read

readNatural :: String -> NN.Natural
readNatural = fromInteger . readInteger

readUnary :: String -> Unary
readUnary = read

main :: IO ()
main = do
  args <- SE.getArgs

  if args == ["-t"] then
    QC.quickCheck propReversible
  else
    interact (unlines . map (show . Unary . readNatural) . lines)
