{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Data.List (sortBy, intersect)
import Coordinate

-- relative movement
data WirePath = WirePath { movements :: [Coordinate]
                         } deriving (Show, Eq)

-- absolute coordinate pairs
data WireVisits = WireVisits { coordinates :: [Coordinate]
                             } deriving (Show, Eq)

parseWirePath :: String -> WirePath
parseWirePath [] = WirePath { movements = [Coordinate 0 0] }
parseWirePath s =
  let commands = wordsWhen (== ',') s
      movements = map parseMovement commands
  in WirePath { movements = movements }

parseMovement :: String -> Coordinate
parseMovement (c:cs) | c == 'R' = Coordinate (read cs :: Int) 0
                     | c == 'L' = Coordinate (negate $ read cs :: Int) 0
                     | c == 'U' = Coordinate 0 (read cs :: Int)
                     | c == 'D' = Coordinate 0 (negate $ read cs :: Int)
                     | otherwise = Coordinate 0 0

-- split string based on condition, e.g. wordsWhen (== ',') s
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

-- generate list of coordinates between start (exclusive) and end (inclusive) coordinates
walkLine :: Coordinate -> Coordinate -> [Coordinate]
walkLine start Coordinate {x=0, y=0} = []
walkLine start Coordinate {x=0, y=ye} =
  let step = if ye < 0 then -1 else 1
      start' = Coordinate (x start) (step + (y start))
      end = Coordinate 0 (ye - step)
  in start' : walkLine start' end
walkLine start Coordinate {x=xe, y=0} =
  let step = if xe < 0 then -1 else 1
      start' = Coordinate (step + (x start)) (y start)
      end = Coordinate (xe - step) 0
  in start' : walkLine start' end

walkPath :: Coordinate -> [Coordinate] -> [Coordinate]
walkPath start [] = []
walkPath start (p:ps) =
  let line = walkLine start p
      start' = last line
  in line ++ (walkPath start' ps)

constructWireVisits :: WirePath -> WireVisits
constructWireVisits wp =
  let cs = walkPath (Coordinate 0 0) (movements wp)
  in WireVisits { coordinates = cs }

compareManhattanDistance c1 c2
  | d1 < d2 = GT
  | d1 > d2 = LT
  | d1 == d2 = EQ
  where
    d1 = manhattanDistance c1
    d2 = manhattanDistance c2

sortWireVisits :: WireVisits -> WireVisits
sortWireVisits wv = WireVisits { coordinates = sortBy (flip compareManhattanDistance) (coordinates wv) }

findCommonVisits :: WireVisits -> WireVisits -> WireVisits
findCommonVisits wv1 wv2 =
  let swv1 = sortWireVisits wv1
      swv2 = sortWireVisits wv2
  in WireVisits { coordinates = intersect (coordinates swv1) (coordinates swv2) }

-- return the manhattan distance of the closest intersection
part1 :: [String] -> Int
part1 ss =
  let wp1 = parseWirePath $ ss !! 0
      wp2 = parseWirePath $ ss !! 1
      wv1 = constructWireVisits wp1
      wv2 = constructWireVisits wp2
      swv1 = sortWireVisits wv1
      swv2 = sortWireVisits wv2
      common = findCommonVisits swv1 swv2
  in manhattanDistance . head . coordinates $ common

