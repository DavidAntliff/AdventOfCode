module Lib where
import qualified Data.Set as Set

-- remove leading '+' characters from input strings
stripPlus :: String -> String
stripPlus ('+':x) = x
stripPlus x = x


calibrate :: (Num a, Read a) => [String] -> a
calibrate = sum . map read . map stripPlus


-- Part 2: Given a list of strings, iterate cyclically calculating
-- the current calibration value, adding it to a set, and terminating
-- when the set already contains the value.


-- Calculate the next sum, add to the set, and return a flag
-- indicating if it is already a member of the set
nextValue :: (Num a, Read a, Ord a) => a -> Set.Set a -> a -> (Bool, a, Set.Set a)
nextValue current seen value =
  let
    current' = current + value
  in if Set.member current' seen
    then (True, current', Set.insert current' seen)
    else (False, current', Set.insert current' seen)


-- Recursively process the next value from the input stream.
-- Terminate if nextValue indicates it has seen the total before.
addNextValue :: (Num a, Read a, Ord a) => a -> Set.Set a -> [a] -> a
addNextValue current seen (x:values) =
  let
    (found, current', seen') = nextValue current seen x
  in if found
    then current'
    else addNextValue current' seen' values


-- Start with an accumulator value of 0, and mark it as "seen" in the set.
findFirstDuplicate :: (Num a, Ord a, Read a) => [String] -> a
findFirstDuplicate =
  addNextValue 0 (Set.singleton 0) . cycleValues


-- Construct a cyclic list of calibration values from a stream of input strings
cycleValues :: (Num a, Read a) => [String] -> [a]
cycleValues = cycle . map read . map stripPlus

