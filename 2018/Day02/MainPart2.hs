module Main where
import Day02 ()

main = interact sumFile
  where
    sumFile = show . id . lines
