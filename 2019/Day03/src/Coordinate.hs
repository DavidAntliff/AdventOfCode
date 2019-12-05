module Coordinate where

import Prelude hiding (sum)

data Coordinate = Coordinate { x :: Int
                             , y :: Int
                             } deriving (Show, Eq)

sum :: Coordinate -> Coordinate -> Coordinate
sum c1 c2 = Coordinate { x = (x c1) + (x c2), y = (y c1) + (y c2) }

manhattanDistance :: Coordinate -> Int
manhattanDistance c = (abs $ x c) + (abs $ y c)
