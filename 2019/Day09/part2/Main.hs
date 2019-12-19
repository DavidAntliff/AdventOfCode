module Main where

import Lib

main = interact $ (++ "\n") . show . part2 . lines
