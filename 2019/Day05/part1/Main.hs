module Main where

import Lib

main = interact $ show . runProgram . (setInput [1]) . loadProgram . lines
