module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.List.Split (splitOn)

import Data.Sequence (Seq(..), index)
import qualified Data.Sequence as Seq

type Program = (Seq Int, Int, [Int], [Int])  -- memory, program counter, input, output (reversed)

-- Parameter Modes
--  0: position mode - parameter values are indexes to memory locations
--  1: immediate mode - parameter values are literal

runProgram :: Program -> Program
runProgram (mem, pc, input, output) =
  let handler = case (loadImmediate mem pc) `mod` 100 of
                  1 -> runProgram . addInstruction
                  2 -> runProgram . multInstruction
                  3 -> runProgram . opcode3Instruction  -- read from input
                  4 -> runProgram . opcode4Instruction  -- write to output
                  99 -> terminateInstruction
  in handler (mem, pc, input, output)

loadPosition :: Seq Int -> Int -> Int
loadPosition mem addr = mem `index` (mem `index` addr)

loadImmediate :: Seq Int -> Int -> Int
loadImmediate mem addr = mem `index` addr

-- decodeLoad :: Int -> (Seq Int -> Int -> Int)
-- decodeLoad opcode = loadPosition

addInstruction :: Program -> Program
addInstruction (mem, pc, input, output) =
  let value = loadPosition mem (pc + 1) + loadPosition mem (pc + 2)
  in (Seq.update (loadImmediate mem (pc + 3)) value mem, pc + 4, input, output)

multInstruction :: Program -> Program
multInstruction (mem, pc, input, output) =
  let value = loadPosition mem (pc + 1) * loadPosition mem (pc + 2)
  in (Seq.update (loadImmediate mem (pc + 3)) value mem, pc + 4, input, output)

terminateInstruction :: Program -> Program
terminateInstruction x = x

opcode3Instruction :: Program -> Program
opcode3Instruction (mem, pc, input, output) =
  (Seq.update (loadImmediate mem (pc + 1)) (head input) mem, pc + 2, tail input, output)

opcode4Instruction :: Program -> Program
opcode4Instruction (mem, pc, input, output) =
  (mem, pc + 2, input, (loadPosition mem (pc + 1)) : output)

readMemory :: String -> Seq Int
readMemory s = Seq.fromList $ (mapMaybe readMaybe :: [String] -> [Int]) $ splitOn "," s

loadProgram :: [String] -> Program
loadProgram s = (readMemory $ head s, 0, [], [])  -- single line of input
