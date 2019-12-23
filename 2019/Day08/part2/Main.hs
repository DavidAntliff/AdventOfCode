module Main where

import Lib (part2)

main = interact $ (++ "\n") . part2 . lines
