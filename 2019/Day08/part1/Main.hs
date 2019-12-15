module Main where

import Lib (part1)

main = interact $ (++ "\n") . show . part1 . lines

