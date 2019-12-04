module Main where

import Lib

-- read lines from stdin,
-- convert first line to a Program record,
-- restore gravity assist program,
-- run the program,
-- get the first value from memory,
-- and display it.
main = interact $ show . getValueAtPosition0 . runProgram . restoreGravityAssist . loadProgram . lines
