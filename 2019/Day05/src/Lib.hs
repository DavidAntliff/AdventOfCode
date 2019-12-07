module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.List.Split (splitOn)

import Data.Sequence (Seq(..), index)
import qualified Data.Sequence as Seq

import Debug.Trace (trace)

type Program = (Seq Int, Int, [Int], [Int])  -- memory, program counter, input, output (reversed)

-- Parameter Modes
--  0: position mode - parameter values are indexes to memory locations
--  1: immediate mode - parameter values are literal

runProgram :: Program -> Program
runProgram (mem, pc, input, output) =
  let opcode = loadImmediate mem pc
      --handler = case trace ("opcode " ++ show opcode) (opcode `mod` 100) of
      handler = case opcode `mod` 100 of
                  1 -> runProgram . addInstruction
                  2 -> runProgram . multInstruction
                  3 -> runProgram . opcode3Instruction  -- read from input
                  4 -> runProgram . opcode4Instruction  -- write to output
                  5 -> runProgram . jumpIfTrueInstruction
                  6 -> runProgram . jumpIfFalseInstruction
                  7 -> runProgram . lessThanInstruction
                  8 -> runProgram . equalsInstruction
                  99 -> terminateInstruction
  in handler (mem, pc, input, output)

loadPosition :: Seq Int -> Int -> Int
loadPosition mem addr = loadImmediate mem (loadImmediate mem addr)

loadImmediate :: Seq Int -> Int -> Int
loadImmediate mem addr = mem `index` addr

-- support up to 3 parameters
decodeParamModes :: Int -> [Int]
decodeParamModes opcode = map (flip mod 10) $ map (div opcode) [100, 1000, 10000]

-- support Position (0) and Immediate (1) parameter modes
loadParam :: Seq Int -> Int -> Int -> Int
loadParam mem addr mode = case mode of
                            0 -> loadPosition mem addr
                            1 -> loadImmediate mem addr

addInstruction :: Program -> Program
addInstruction (mem, pc, input, output) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      mem' = Seq.update (loadImmediate mem (pc + 3)) (p1 + p2) mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output)

multInstruction :: Program -> Program
multInstruction (mem, pc, input, output) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      mem' = Seq.update (loadImmediate mem (pc + 3)) (p1 * p2) mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output)

terminateInstruction :: Program -> Program
terminateInstruction x = x

-- store is never immediate
opcode3Instruction :: Program -> Program
opcode3Instruction (mem, pc, input, output) =
  (Seq.update (loadImmediate mem (pc + 1)) (head input) mem, pc + 2, tail input, output)

opcode4Instruction :: Program -> Program
opcode4Instruction (mem, pc, input, output) =
  let (pm1:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      pc' = pc + 2
      output' = p1 : output
  in (mem, pc', input, output')

jumpIfTrueInstruction :: Program -> Program
jumpIfTrueInstruction (mem, pc, input, output) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      pc' = if p1 /= 0 then p2 else pc + 3
  in (mem, pc', input, output)

jumpIfFalseInstruction :: Program -> Program
jumpIfFalseInstruction (mem, pc, input, output) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      pc' = if p1 == 0 then p2 else pc + 3
  in (mem, pc', input, output)

lessThanInstruction :: Program -> Program
lessThanInstruction (mem, pc, input, output) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      result = if p1 < p2 then 1 else 0
      mem' = Seq.update (loadImmediate mem (pc + 3)) result mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output)

equalsInstruction :: Program -> Program
equalsInstruction (mem, pc, input, output) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      result = if p1 == p2 then 1 else 0
      mem' = Seq.update (loadImmediate mem (pc + 3)) result mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output)

readMemory :: String -> Seq Int
readMemory s = Seq.fromList $ (mapMaybe readMaybe :: [String] -> [Int]) $ splitOn "," s

loadProgram :: [String] -> Program
loadProgram s = (readMemory $ head s, 0, [], [])  -- single line of input

setInput :: [Int] -> Program -> Program
setInput input (mem, pc, _, output) = (mem, pc, input, output)
