module Main where
import Day07

main = interact stdin
  where
    stdin = show . part1 . lines
