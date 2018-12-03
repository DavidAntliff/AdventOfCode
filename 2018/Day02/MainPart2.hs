module Main where
import Day02 (findPair)

main = interact sumFile
  where
    sumFile = show . findPair . lines
