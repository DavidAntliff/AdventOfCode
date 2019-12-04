module Main where

import Lib

main = interact $ show . getValueAtPosition0 . runProgram . loadProgram . lines
