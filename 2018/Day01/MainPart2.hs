module Main where
import Day01 (findFirstDuplicate)


main = interact sumFile
  where
    sumFile = show . findFirstDuplicate . lines
