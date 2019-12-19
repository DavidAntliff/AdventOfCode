module Main where

import Lib

main = interact $ (++ "\n") . show . part1 . lines
