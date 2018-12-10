module Main where
import Day05

main = interact stdin
  where
    stdin = show . map reactPolymer . lines
