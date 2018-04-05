-- |
-- Mo provides encoders/decoders for unary representation of natural numbers.
module Mo where

import qualified Numeric.Natural as NN
import qualified Data.Int as DI

-- | Unary holds natural numerical values.
newtype Unary = Unary NN.Natural deriving (Eq, Ord)

-- | nat retrieves numerical values.
nat :: Unary -> NN.Natural
nat (Unary x) = x

instance Show Unary where
  show (Unary v) = replicate ((fromInteger . toInteger) v) '1'

instance Read Unary where
  readsPrec _ ones = [((Unary . fromInteger . toInteger . length . filter (== '1')) ones, "")]

-- | propReversible checks whether unaries are generally encodable and decodable
propReversible :: DI.Int8 -> Bool
propReversible x
  | x < 0 = True
  | otherwise = u == u' where
    u = (Unary . fromInteger . toInteger) x
    u' = ((read :: String -> Unary) . show) u
