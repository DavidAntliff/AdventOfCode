module Main where

import Lib (calibrate)

main = interact sumFile
  where
    sumFile = show . calibrate . lines
