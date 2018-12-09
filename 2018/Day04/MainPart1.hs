module Main where
import Day04

main = interact sumFile
  where
    sumFile = show . calcLongestSleeper . splitRecords . sortRecords . parseRecords . lines
