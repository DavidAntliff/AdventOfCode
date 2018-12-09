module Main where
import Day03 (countOverlaps)

main = interact sumFile
  where
    sumFile = show . countOverlaps . lines
