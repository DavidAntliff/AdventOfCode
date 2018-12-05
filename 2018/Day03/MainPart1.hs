module Main where
import Day03 (testParse)

main = interact sumFile
  where
    sumFile = show . testParse . lines
