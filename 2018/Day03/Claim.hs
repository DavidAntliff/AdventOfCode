module Claim where
import Data.Ord as Ord

data Claim = Claim { id :: Integer
                   , x :: Integer
                   , y :: Integer
                   , w :: Integer
                   , h :: Integer
                   } deriving (Show)

instance Eq Claim where
  (Claim _ x1 y1 w1 h1) == (Claim _ x2 y2 w2 h2) = x1 == x2 && y1 == y2 && w1 == w2 && h1 == h2

instance Ord Claim where
  compare = comparing y <> comparing x
