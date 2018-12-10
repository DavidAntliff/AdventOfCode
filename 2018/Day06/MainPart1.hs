module Main where
import Day06

main = interact stdin
  where
    stdin = show . id . lines
