module Main where
import Day02 (checkSum)

main = interact sumFile
  where
    sumFile = show . checkSum . lines
