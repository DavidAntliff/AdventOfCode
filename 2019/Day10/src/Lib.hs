module Lib where

import Data.List (findIndices)

import Data.Set (Set)
import qualified Data.Set as Set


-- Part 1
--
-- For each asteroid (candidate):
--   - create a map of all asteroid locations relative to the candidate asteroid
--   - for each other asteroid, calculate the simplest rational gradient between the asteroid and the candidate
--     - for each integer multiple of the gradient, up to but not including the candidate, search for other asteroids
--       - if another asteroid exists at a multiple of the gradient, remove the original asteroid from the map
-- Once all asteroids have been considered, the resulting map will contain only visible asteroids


-- To read the map of asteroids, parse line by line, adding asteroids to the overall Map
readAsteroidMap :: String -> Set (Int, Int)
readAsteroidMap s =
  let mapLines = zip [0..] $ lines s
      result = foldl1 Set.union $ map readAsteroidMapLine mapLines
  in result

readAsteroidMapLine :: (Int, String) -> Set (Int, Int)
readAsteroidMapLine (y, s) = Set.fromList $ zip (findIndices (== '#') s) $ repeat y

