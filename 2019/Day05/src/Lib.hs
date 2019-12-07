module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.List.Split (splitOn)

import Data.Sequence (Seq(..), index)
import qualified Data.Sequence as Seq

type Program = (Seq Int, Int)  -- memory, program counter

runProgram :: Program -> Program
runProgram (mem, pc) =
  let instruction = mem `index` pc
      handler = case mem `index` pc of
                  1 -> runProgram . addInstruction
                  2 -> runProgram . multInstruction
                  99 -> terminateInstruction
  in handler (mem, pc)

addInstruction :: Program -> Program
addInstruction (mem, pc) =
  let index1 = mem `index` (pc + 1)
      index2 = mem `index` (pc + 2)
      value = mem `index` index1 + mem `index` index2
      dest = mem `index` (pc + 3)
  in (Seq.update dest value mem, pc + 4)

multInstruction :: Program -> Program
multInstruction (mem, pc) =
  let index1 = mem `index` (pc + 1)
      index2 = mem `index` (pc + 2)
      value = mem `index` index1 * mem `index` index2
      dest = mem `index` (pc + 3)
  in (Seq.update dest value mem, pc + 4)

terminateInstruction :: Program -> Program
terminateInstruction x = x

readInput :: String -> Seq Int
readInput s = Seq.fromList $ (mapMaybe readMaybe :: [String] -> [Int]) $ splitOn "," s

loadProgram :: [String] -> Program
loadProgram s =
  let mem = readInput $ head s  -- single line of input
  in (mem, 0)
