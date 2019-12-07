module Main where

import Lib

main = interact $ show . runProgram . loadProgram . lines
