module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

calcFuel :: Int -> Int
calcFuel x = x `div` 3 - 2

toIntList :: [String] -> [Int]
toIntList x = (mapMaybe readMaybe :: [String] -> [Int]) x

sumFuel :: [String] -> Int
sumFuel x = sum . map calcFuel . toIntList $ x
