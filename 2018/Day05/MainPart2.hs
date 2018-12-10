module Main where
import Day05

main = interact stdin
  where
    stdin = show . part2 . lines
