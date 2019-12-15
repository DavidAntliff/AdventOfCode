module Main where

import Lib (part2)

main = interact $ (++ "\n") . show . part2 . lines
