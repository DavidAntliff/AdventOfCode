module Main where
import Day02 (findPair, findCommon)

main = interact sumFile
  where
    sumFile = show . findCommon . findPair . lines
