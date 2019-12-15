module Lib where

import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.List (minimumBy)

-- input is 15,000 characters in length,
-- image is 25 x 6 = 150 characters,
-- therefore there are 100 layers

image_width :: Int
image_width = 25

image_height :: Int
image_height = 6

image_length :: Int
image_length = image_width * image_height

-- Find the layer that contains the fewest 0 digits.
-- On that layer, what is the number of 1 digits multiplied by the number of 2 digits?
part1 :: [String] -> Int
part1 ss =
  let image_data = head ss
      layers = chunksOf image_length image_data
      counts_per_layer = map count012 layers  -- returns (#0, #1, #2)
      minimum0 = minimumBy (compare `on` get1st) counts_per_layer
  in (get2nd minimum0) * (get3rd minimum0)

count012 :: String -> (Int, Int, Int)
count012 s =
  let count0 = countChar '0' s
      count1 = countChar '1' s
      count2 = countChar '2' s
  in (count0, count1, count2)

countChar :: Char -> String -> Int
countChar c s = foldr (\x acc -> if x == c then acc + 1 else acc) 0 s

get1st :: (a, b, c) -> a
get1st (a, _, _) = a

get2nd :: (a, b, c) -> b
get2nd (_, b, _) = b

get3rd :: (a, b, c) -> c
get3rd (_, _, c) = c

-- Part 2
part2 :: [String] -> Int
part2 ss = 0

