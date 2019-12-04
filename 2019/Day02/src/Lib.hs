module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

data Program = Program { memory :: [Int]
                       , counter :: Int
                       } deriving (Show)

instance Eq Program where
  (Program m1 c1) == (Program m2 c2) = m1 == m2 && c1 == c2

runProgram :: Program -> Program
runProgram p
  | memory p !! counter p == 1 = runProgram $ addInstruction p
  | memory p !! counter p == 2 = runProgram $ multInstruction p
--  | memory p !! counter p == 99 = p
  | otherwise = p

addInstruction :: Program -> Program
addInstruction p =
  let pc = counter p
      index1 = memory p !! (pc + 1)
      index2 = memory p !! (pc + 2)
      value = memory p !! index1 + memory p !! index2
      dest = memory p !! (pc + 3)
      (x, _:ys) = splitAt dest $ memory p
  in Program {memory=x ++ value : ys, counter = pc + 4}

multInstruction :: Program -> Program
multInstruction p =
  let pc = counter p
      index1 = memory p !! (pc + 1)
      index2 = memory p !! (pc + 2)
      value = memory p !! index1 * memory p !! index2
      dest = memory p !! (pc + 3)
      (x, _:ys) = splitAt dest $ memory p
  in Program {memory=x ++ value : ys, counter = pc + 4}

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

readInput :: String -> [Int]
readInput s = (mapMaybe readMaybe :: [String] -> [Int]) $ wordsWhen (== ',') s

replace :: Int -> Int -> [Int] -> [Int]
replace pos val mem =
  let (x, _:ys) = splitAt pos $ mem
  in x ++ val : ys

restoreGravityAssist :: [Int] -> [Int]
restoreGravityAssist m = replace 2 2 . replace 1 12 $ m

loadProgram :: [String] -> Program
loadProgram s =
  let
    mem = restoreGravityAssist $ readInput $ s !! 0  -- single line of input
  in Program {memory=mem, counter=0}


getValueAtPosition0 :: Program -> Int
getValueAtPosition0 p = memory p !! 0
