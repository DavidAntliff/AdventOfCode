module Lib where
import Data.List.Split (splitOn)
import Data.List (group)

input = "193651-649729"

notDecreasing :: (Ord a) => [a] -> Bool
notDecreasing [] = True
notDecreasing [x] = True
notDecreasing (x:y:ys) = x <= y && notDecreasing (y:ys)

validPassword :: String -> Bool
validPassword x =
  let lengthSix = length x == 6
      doublePresent = any ((>=2) . length) $ group x
  in lengthSix && doublePresent && notDecreasing x

part1 :: Int
part1 =
  let [start, end] = (map read $ splitOn "-" input) :: [Int]
  in length $ filter validPassword $ map show $ [start..end]

