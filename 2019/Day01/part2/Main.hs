module Main where

import Lib (sumFuel2)

main = interact $ show . sumFuel2 . lines
