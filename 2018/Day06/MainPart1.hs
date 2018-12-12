module Main where
import Day06

main = interact stdin
  where
    stdin = show . part1 . lines
