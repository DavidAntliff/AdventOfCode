{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Data.List (sortBy, sort, intersect, elemIndex)
import Data.Maybe (fromJust)
import Linear.V2
import qualified Data.Set as Set

parseWirePath :: String -> [V2 Int]
parseWirePath [] = [V2 0 0]
parseWirePath s =
  let commands = wordsWhen (== ',') s
  in map parseMovement commands

parseMovement :: String -> V2 Int
parseMovement (c:cs) | c == 'R' = V2 (read cs :: Int) 0
                     | c == 'L' = V2 (negate $ read cs :: Int) 0
                     | c == 'U' = V2 0 (read cs :: Int)
                     | c == 'D' = V2 0 (negate $ read cs :: Int)
                     | otherwise = V2 0 0

-- split string based on condition, e.g. wordsWhen (== ',') s
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

-- generate list of coordinates between start (exclusive) and end (inclusive) coordinates
walkLine :: V2 Int -> V2 Int -> [V2 Int]
walkLine start (V2 0 0) = []
walkLine start (V2 0 ye) =
  let step = if ye < 0 then -1 else 1
      start' = start + (V2 0 step)
      end = V2 0 (ye - step)
  in start' : walkLine start' end
walkLine start (V2 xe 0) =
  let step = if xe < 0 then -1 else 1
      start' = start + (V2 step 0)
      end = V2 (xe - step) 0
  in start' : walkLine start' end

walkPath :: V2 Int -> [V2 Int] -> [V2 Int]
walkPath start [] = []
walkPath start (p:ps) =
  let line = walkLine start p
      start' = last line
  in line ++ (walkPath start' ps)

constructWireVisits :: [V2 Int] -> Set.Set (V2 Int)
constructWireVisits wp = Set.fromList $ walkPath (V2 0 0) wp

manhattanDistance :: V2 Int -> Int
manhattanDistance = sum . abs

compareManhattanDistance c1 c2
  | d1 < d2 = GT
  | d1 > d2 = LT
  | d1 == d2 = EQ
  where
    d1 = manhattanDistance c1
    d2 = manhattanDistance c2

sortWireVisits :: [V2 Int] -> Set.Set (V2 Int)
sortWireVisits wv = Set.fromList $ sortBy (flip compareManhattanDistance) wv

findCommonVisits :: Set.Set (V2 Int) -> Set.Set (V2 Int) -> Set.Set (V2 Int)
findCommonVisits wv1 wv2 = Set.intersection wv1 wv2

-- return the manhattan distance of the closest intersection
part1 :: [String] -> Int
part1 ss =
  let wp1 = parseWirePath $ ss !! 0
      wp2 = parseWirePath $ ss !! 1
      wv1 = constructWireVisits wp1
      wv2 = constructWireVisits wp2
      common = findCommonVisits wv1 wv2
  in minimum $ Set.map manhattanDistance common


-- Part 2

calcDistance :: [V2 Int] -> [V2 Int] -> V2 Int -> Int
calcDistance c1 c2 c =
  let d1 = fromJust $ elemIndex c c1
      d2 = fromJust $ elemIndex c c2
  in d1 + d2

calcDistances :: [V2 Int] -> [V2 Int] -> [V2 Int] -> [Int]
calcDistances c1 c2 [] = []
calcDistances c1 c2 (c:cs) = (calcDistance c1 c2 c) : (calcDistances c1 c2 cs)

part2 :: [String] -> Int
part2 ss =
  let wp1 = parseWirePath $ ss !! 0
      wp2 = parseWirePath $ ss !! 1
      wv1 = constructWireVisits wp1
      wv2 = constructWireVisits wp2
      common = findCommonVisits wv1 wv2
      distance = 0
  in distance

