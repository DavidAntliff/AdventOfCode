module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.List.Split (splitOn)

import Data.Sequence (Seq(..), index, (><))
import qualified Data.Sequence as Seq

import Debug.Trace (trace)
--trace :: a -> b -> b
--trace _ y = y

data State = Running | Blocked | Terminated deriving (Eq, Show)

type Program = (Seq Int, Int, Int, [Int], [Int], State)  -- memory, program counter, relative_base, input, output (reversed), State

-- Parameter Modes
--  0: position mode - parameter values are indexes to memory locations
--  1: immediate mode - parameter values are literal
--  2: relative mode - parameter values are indexes to memory locations, relative to relbase

runProgram :: Program -> Program
runProgram (mem, pc, relbase, input, output, state) =
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
                  9 -> adjustRelbaseInstruction
                  99 -> terminateInstruction
                  _ -> error ("Unhandled opcode " ++ show opcode ++ " at address " ++ show pc)
      program' = handler (mem, pc, relbase, input, output, state)
  in case state of
    Running -> runProgram program'
    Blocked -> program'
    Terminated -> program'

loadPosition :: Seq Int -> Int -> Int
loadPosition mem addr = loadImmediate mem (loadImmediate mem addr)

-- "parameters that an instruction writes to will never be in immediate mode" - Day 5
loadImmediate :: Seq Int -> Int -> Int
loadImmediate mem addr = mem `index` addr

loadRelative :: Seq Int -> Int -> Int -> Int
loadRelative mem addr relbase =
  let relAddr = loadImmediate mem addr
      loadAddr = relbase + relAddr
      value = loadImmediate mem loadAddr
  in
    --trace ("loadRelative: mem " ++ show mem ++ ", addr " ++ show addr ++ ", relbase " ++ show relbase ++
    --       ", relAddr " ++ show relAddr ++ ", loadAddr " ++ show loadAddr ++ ", value " ++ show value)
    value

-- support up to 3 parameters
decodeParamModes :: Int -> [Int]
decodeParamModes opcode = map (flip mod 10) $ map (div opcode) [100, 1000, 10000]

-- support Position (0), Immediate (1) and Relative (2) parameter modes
loadParam :: Seq Int -> Int -> Int -> Int -> Int
loadParam mem addr relbase mode =
  --trace ("loadParam: mem " ++ show mem ++ ", addr " ++ show addr ++ ", relbase " ++ show relbase ++ ", mode " ++ show mode)
  (case mode of
    0 -> loadPosition mem addr
    1 -> loadImmediate mem addr
    2 -> loadRelative mem addr relbase
    _ -> error ("loadParam: Unhandled parameter mode " ++ show mode))

storeParam :: Seq Int -> Int -> Int -> Int -> Int -> Seq Int
storeParam mem addr relbase mode value =
  --trace ("storeParam: mem " ++ show mem ++ ", addr " ++ show addr ++ ", relbase " ++ show relbase ++ ", mode " ++ show mode ++ ", value " ++ show value)
  (case mode of
     0 -> storePosition mem addr value
     -- Never immediate
     2 -> storeRelative mem addr relbase value
     _ -> error ("storeParam: Unhandled parameter mode " ++ show mode))

storePosition :: Seq Int -> Int -> Int -> Seq Int
storePosition mem addr value = Seq.update (loadImmediate mem addr) value mem

storeRelative :: Seq Int -> Int -> Int -> Int -> Seq Int
storeRelative mem addr relbase value = Seq.update ((loadImmediate mem addr) + relbase) value mem

addInstruction :: Program -> Program
addInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:pm2:pm3:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      p2 = loadParam mem (pc + 2) relbase pm2
      mem' = storeParam mem (pc + 3) relbase pm3 (p1 + p2)
      pc' = pc + 4
  in (mem', pc', relbase, input, output, state)

multInstruction :: Program -> Program
multInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:pm2:pm3:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      p2 = loadParam mem (pc + 2) relbase pm2
      mem' = storeParam mem (pc + 3) relbase pm3 (p1 * p2)
      pc' = pc + 4
  in (mem', pc', relbase, input, output, state)

terminateInstruction :: Program -> Program
terminateInstruction (mem, pc, relbase, input, output, _) = (mem, pc, relbase, input, output, Terminated)

-- take input value from head of input list
-- (store is never immediate)
inputInstruction :: Program -> Program
inputInstruction (mem, pc, relbase, [], output, _) = (mem, pc, relbase, [], output, Blocked)
inputInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:_) = decodeParamModes $ loadImmediate mem pc
      mem' = storeParam mem (pc + 1) relbase pm1 (head input)
  in (mem', pc + 2, relbase, tail input, output, state)

-- add output value to head of output list
outputInstruction :: Program -> Program
outputInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      pc' = pc + 2
      output' = p1 : output
  in
    --trace ("output " ++ show p1)
    (mem, pc', relbase, input, output', state)

jumpIfTrueInstruction :: Program -> Program
jumpIfTrueInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      p2 = loadParam mem (pc + 2) relbase pm2
      pc' = if p1 /= 0 then p2 else pc + 3
  in (mem, pc', relbase, input, output, state)

jumpIfFalseInstruction :: Program -> Program
jumpIfFalseInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:pm2:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      p2 = loadParam mem (pc + 2) relbase pm2
      pc' = if p1 == 0 then p2 else pc + 3
  in (mem, pc', relbase, input, output, state)

lessThanInstruction :: Program -> Program
lessThanInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:pm2:pm3:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      p2 = loadParam mem (pc + 2) relbase pm2
      result = if p1 < p2 then 1 else 0
      mem' = storeParam mem (pc + 3) relbase pm3 result
      pc' = pc + 4
  in (mem', pc', relbase, input, output, state)

equalsInstruction :: Program -> Program
equalsInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:pm2:pm3:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      p2 = loadParam mem (pc + 2) relbase pm2
      result = if p1 == p2 then 1 else 0
      mem' = storeParam mem (pc + 3) relbase pm3 result
      pc' = pc + 4
  in (mem', pc', relbase, input, output, state)

adjustRelbaseInstruction :: Program -> Program
adjustRelbaseInstruction (mem, pc, relbase, input, output, state) =
  let (pm1:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) relbase pm1
      pc' = pc + 2
  in
    (mem, pc', relbase + p1, input, output, state)

readMemory :: String -> Seq Int
readMemory s = Seq.fromList $ (mapMaybe readMaybe :: [String] -> [Int]) $ splitOn "," s

loadProgram :: String -> Program
loadProgram s = (readMemory s, 0, 0, [], [], Running)  -- single line of input

-- replace the input list with the new list, clear Blocked state
setInput :: [Int] -> Program -> Program
setInput input (mem, pc, relbase, _, output, _) = (mem, pc, relbase, input, output, Running)

-- add new input value to the end of the input list, clear Blocked state
addInput :: Int -> Program -> Program
addInput new_input (mem, pc, relbase, input, output, _) = (mem, pc, relbase, input ++ [new_input], output, Running)

getState :: Program -> State
getState (_, _, _, _, _, state) = state

getOutput :: Program -> [Int]
getOutput (_, _, _, _, output, _) = output

extendMemory :: Int -> Program -> Program
extendMemory new_size (mem, pc, relbase, input, output, state) =
  let current_size = length mem
      extension_size = new_size - current_size
      -- if extension_size is negative, take will return []
  in (mem >< Seq.fromList (take extension_size $ repeat 0), pc, relbase, input, output, state)

getRelBase :: Program -> Int
getRelBase (_, _, relbase, _, _, _) = relbase

-- Day 09 Part 1

part1 :: [String] -> Int
part1 ss =
  let program = extendMemory 10240 $ loadProgram $ head ss
      program' = setInput [1] program
      final = runProgram program'
      output = trace (show final) (getOutput final)
  in head output

-- TODO: instruction 9 is done, now need to add the relative addressing mode to all other instructions


-- Day 09 Part 2

part2 :: [String] -> Int
part2 ss =
  let program = extendMemory 10240 $ loadProgram $ head ss
      program' = setInput [2] program
      final = runProgram program'
      output = trace (show final) (getOutput final)
  in head output
