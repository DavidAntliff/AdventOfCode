module Main where

import Lib (findFirstDuplicate)


main = interact sumFile
  where
    sumFile = show . findFirstDuplicate . lines
