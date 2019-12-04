module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)


-- Part 1

calcFuel :: Int -> Int
calcFuel x = x `div` 3 - 2

toIntList :: [String] -> [Int]
toIntList x = (mapMaybe readMaybe :: [String] -> [Int]) x

sumFuel :: [String] -> Int
sumFuel x = sum . map calcFuel . toIntList $ x



-- Part 2

sumFuel2 :: [String] -> Int
sumFuel2 x = sum . map calcFuel2 . toIntList $ x

calcFuel2 :: Int -> Int
calcFuel2 mass
  | fuel < 0 = 0
  | otherwise = fuel + calcFuel2 fuel
  where fuel = mass `div` 3 - 2
