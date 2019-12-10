module Main where

import Lib

main = interact $ show . runProgram . (setInput [5]) . loadProgram . lines
