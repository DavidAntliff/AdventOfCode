module Main where

import Lib (sumFuel)

main = interact $ show . sumFuel . lines
