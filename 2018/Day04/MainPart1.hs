module Main where
import Day04 ()

main = interact sumFile
  where
    sumFile = show . id . lines
