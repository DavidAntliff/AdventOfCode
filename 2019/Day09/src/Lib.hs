module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.List.Split (splitOn)

import Data.Sequence (Seq(..), index)
import qualified Data.Sequence as Seq

import Debug.Trace (trace)

data State = Running | Blocked | Terminated deriving (Eq, Show)

type Program = (Seq Int, Int, [Int], [Int], State)  -- memory, program counter, input, output (reversed), State

-- Parameter Modes
--  0: position mode - parameter values are indexes to memory locations
--  1: immediate mode - parameter values are literal

runProgram :: Program -> Program
runProgram (mem, pc, input, output, state) =
  let opcode = loadImmediate mem pc
      handler = case
        --trace ("pc " ++ show pc ++ ", opcode " ++ show opcode)
        (opcode `mod` 100) of
                  1 -> addInstruction
                  2 -> multInstruction
                  3 -> inputInstruction  -- read from input
                  4 -> outputInstruction  -- write to output
                  5 -> jumpIfTrueInstruction
                  6 -> jumpIfFalseInstruction
                  7 -> lessThanInstruction
                  8 -> equalsInstruction
                  99 -> terminateInstruction
                  _ -> error ("Unhandled opcode " ++ show opcode)
      program' = handler (mem, pc, input, output, state)
  in case state of
    Running -> runProgram program'
    Blocked -> program'
    Terminated -> program'

loadPosition :: Seq Int -> Int -> Int
loadPosition mem addr = loadImmediate mem (loadImmediate mem addr)

-- "parameters that an instruction writes to will never be in intermediate mode" - Day 5
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
                            _ -> error ("Unhandled parameter mode " ++ show mode)

addInstruction :: Program -> Program
addInstruction (mem, pc, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      mem' = Seq.update (loadImmediate mem (pc + 3)) (p1 + p2) mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output, state)

multInstruction :: Program -> Program
multInstruction (mem, pc, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      mem' = Seq.update (loadImmediate mem (pc + 3)) (p1 * p2) mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output, state)

terminateInstruction :: Program -> Program
terminateInstruction (mem, pc, input, output, _) = (mem, pc, input, output, Terminated)

-- take input value from head of input list
-- (store is never immediate)
inputInstruction :: Program -> Program
inputInstruction (mem, pc, [], output, _) = (mem, pc, [], output, Blocked)
inputInstruction (mem, pc, input, output, state) =
  --trace ("input " ++ show (head input))
  (Seq.update (loadImmediate mem (pc + 1)) (head input) mem, pc + 2, tail input, output, state)

-- add output value to head of output list
outputInstruction :: Program -> Program
outputInstruction (mem, pc, input, output, state) =
  let (pm1:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      pc' = pc + 2
      output' = p1 : output
  in
    --trace ("output " ++ show p1)
    (mem, pc', input, output', state)

jumpIfTrueInstruction :: Program -> Program
jumpIfTrueInstruction (mem, pc, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      pc' = if p1 /= 0 then p2 else pc + 3
  in (mem, pc', input, output, state)

jumpIfFalseInstruction :: Program -> Program
jumpIfFalseInstruction (mem, pc, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      pc' = if p1 == 0 then p2 else pc + 3
  in (mem, pc', input, output, state)

lessThanInstruction :: Program -> Program
lessThanInstruction (mem, pc, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      result = if p1 < p2 then 1 else 0
      mem' = Seq.update (loadImmediate mem (pc + 3)) result mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output, state)

equalsInstruction :: Program -> Program
equalsInstruction (mem, pc, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      p2 = loadParam mem (pc + 2) pm2
      result = if p1 == p2 then 1 else 0
      mem' = Seq.update (loadImmediate mem (pc + 3)) result mem  -- never immediate
      pc' = pc + 4
  in (mem', pc', input, output, state)

readMemory :: String -> Seq Int
readMemory s = Seq.fromList $ (mapMaybe readMaybe :: [String] -> [Int]) $ splitOn "," s

loadProgram :: [String] -> Program
loadProgram s = (readMemory $ head s, 0, [], [], Running)  -- single line of input

-- replace the input list with the new list, clear Blocked state
setInput :: [Int] -> Program -> Program
setInput input (mem, pc, _, output, _) = (mem, pc, input, output, Running)

-- add new input value to the end of the input list, clear Blocked state
addInput :: Int -> Program -> Program
addInput new_input (mem, pc, input, output, _) = (mem, pc, input ++ [new_input], output, Running)

getState :: Program -> State
getState (_, _, _, _, state) = state


-- Day 09 Part 1

part1 :: [String] -> Int
part1 ss =
  let program = loadProgram ss
  in 0




-- Day 09 Part 2

part2 :: [String] -> Int
part2 ss =
  let program = loadProgram ss
  in 0
