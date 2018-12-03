module Main where
import Day03 ()

main = interact sumFile
  where
    sumFile = show . id . lines
