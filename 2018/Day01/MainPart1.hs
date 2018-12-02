module Main where

import Day01 (calibrate)

main = interact sumFile
  where
    sumFile = show . calibrate . lines
