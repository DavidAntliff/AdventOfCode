module Main where

import Lib

-- read lines from stdin,
-- convert first line to a Program record,
-- try running the program with combinations of noun, verb values,
-- stop when 19690720 is first value in memory,
-- encode the noun, verb that produces this result,
-- and display it.
main = interact $ show . encode . findNounVerb . loadProgram . lines
