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

restoreGravityAssist :: Program -> Program
restoreGravityAssist p =
  let mem = replace 2 2 . replace 1 12 $ memory p
  in Program {memory=mem, counter=counter p}

loadProgram :: [String] -> Program
loadProgram s =
  let mem = readInput $ s !! 0  -- single line of input
  in Program {memory=mem, counter=0}


getValueAtPosition0 :: Program -> Int
getValueAtPosition0 p = memory p !! 0


-- Part 2


installNounVerb :: Int -> Int -> Program -> Program
installNounVerb n v p =
  let mem = replace 2 v . replace 1 n $ memory p
  in Program {memory=mem, counter=counter p}

tryNounVerb :: Int -> Int -> Program -> Int
tryNounVerb n v p = getValueAtPosition0 . runProgram $ installNounVerb n v p

check :: (Int, Int, Int) -> Bool
check (_, _, x) = x == 19690720

calcNounVerb :: Int -> Int -> Program -> (Int, Int, Int)
calcNounVerb n v p = (n, v, tryNounVerb n v p)

findNounVerb :: Program -> (Int, Int, Int)
findNounVerb p =
  let nouns = [0..99]
      verbs = [0..99]
      space = [(n, v) | n <- nouns, v <- verbs]
  in last $ takeUntil (check) [calcNounVerb (fst s) (snd s) p | s <- space]

-- Construct list of items from a foldable until condition is true.
-- Final list will contain the first true item.
takeUntil :: Foldable t => (a -> Bool) -> t a -> [a]
takeUntil p = foldr (\x r -> if (not (p x)) then (x:r) else [x]) []

encode :: Num a => (a, a, b) -> a
encode (n, v, _) = 100 * n + v
