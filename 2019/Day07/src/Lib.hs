module Lib where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.List.Split (splitOn)
import Data.List (permutations)

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
        trace ("pc " ++ show pc ++ ", opcode " ++ show opcode)
        (opcode `mod` 100) of
                  1 -> addInstruction
                  2 -> multInstruction
                  3 -> opcode3Instruction  -- read from input
                  4 -> opcode4Instruction  -- write to output
                  5 -> jumpIfTrueInstruction
                  6 -> jumpIfFalseInstruction
                  7 -> lessThanInstruction
                  8 -> equalsInstruction
                  99 -> terminateInstruction
      program' = handler (mem, pc, input, output, state)
  in case state of
    Running -> runProgram program'
    Blocked -> program'
    Terminated -> program'

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
terminateInstruction (mem, pc, input, output, state) = (mem, pc, input, output, Terminated)

-- take input value from head of input list
-- store is never immediate
opcode3Instruction :: Program -> Program
opcode3Instruction (mem, pc, [], output, state) = (mem, pc, [], output, Blocked)
opcode3Instruction (mem, pc, input, output, state) =
  trace ("input " ++ show (head input))
  (Seq.update (loadImmediate mem (pc + 1)) (head input) mem, pc + 2, tail input, output, state)

-- add output value to head of output list
opcode4Instruction :: Program -> Program
opcode4Instruction (mem, pc, input, output, state) =
  let (pm1:_) = decodeParamModes $ loadImmediate mem pc
      p1 = loadParam mem (pc + 1) pm1
      pc' = pc + 2
      output' = p1 : output
  in
    trace ("output " ++ show p1)
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
setInput input (mem, pc, _, output, state) = (mem, pc, input, output, Running)

-- add new input value to the end of the input list, clear Blocked state
addInput :: Int -> Program -> Program
addInput new_input (mem, pc, input, output, state) = (mem, pc, input ++ [new_input], output, Running)


-- Day 07
-- Part 1

-- 1. generate all permutations of [0, 1, 2, 3, 4]
-- 2. for each permutation, run the chain of amplifiers with input value 0
-- 3. keep track of the outputs
-- 4. take the maximum output and output it

part1 :: [String] -> Int
part1 ss =
  let program = loadProgram ss
      perms = permutations [0, 1, 2, 3, 4]
      results = map (runChain program) perms
  in maximum results

-- run the entire chain once
runChain :: Program -> [Int] -> Int
runChain prog phases =
  let output1 = runAmp prog (phases !! 0) 0
      output2 = runAmp prog (phases !! 1) output1
      output3 = runAmp prog (phases !! 2) output2
      output4 = runAmp prog (phases !! 3) output3
      output5 = runAmp prog (phases !! 4) output4
  in output5

runAmp :: Program -> Int -> Int -> Int
runAmp prog phase input =
  let prog' = setInput [phase, input] prog
      (_, _, _, result, _) = runProgram prog'
  in head result


-- each Amp requires 10 inputs
part2 :: [String] -> Int
part2 ss =
  let program = loadProgram ss
      perms = permutations [5 .. 9]
      results = map (runFeedback program) perms
  in maximum results

-- need to runProgram until input requested (and none available),
-- or program terminates.

-- run the entire chain until the fifth amp terminates,
-- and return the final value from the fifth amp
runFeedback :: Program -> [Int] -> Int
runFeedback program phases =
  let amps0 = trace ("phases " ++ show phases) (initAmps program phases)
      amps0' = executeChain amps0 0
      amps1' = executeChain amps0' (getFinalOutput amps0')
      amps2' = executeChain amps1' (getFinalOutput amps1')
      amps3' = executeChain amps2' (getFinalOutput amps2')
      amps4' = executeChain amps3' (getFinalOutput amps3')
      amps5' = executeChain amps4' (getFinalOutput amps4')
      amps6' = executeChain amps5' (getFinalOutput amps5')
      amps7' = executeChain amps6' (getFinalOutput amps6')
      amps8' = executeChain amps7' (getFinalOutput amps7')
      amps9' = executeChain amps8' (getFinalOutput amps8')
      amps10' = executeChain amps9' (getFinalOutput amps9')
      thrusters = getFinalOutput amps9'
  in
    trace ("thrusters " ++ show thrusters)
    thrusters

getFinalOutput :: [Program] -> Int
getFinalOutput amps =
  let (_, _, _, last_output, _) = last amps
  in head last_output

executeChain :: [Program] -> Int -> [Program]
executeChain amps input =
  let amp0'@(_,_,_,out0,_) = runAmp2 (amps !! 0) input
      amp1'@(_,_,_,out1,_) = runAmp2 (amps !! 1) (head out0)
      amp2'@(_,_,_,out2,_) = runAmp2 (amps !! 2) (head out1)
      amp3'@(_,_,_,out3,_) = runAmp2 (amps !! 3) (head out2)
      amp4'@(_,_,_,out4,_) = runAmp2 (amps !! 4) (head out3)
  in [amp0', amp1', amp2', amp3', amp4']


initAmps :: Program -> [Int] -> [Program]
initAmps program phases =
  map (\phase -> setInput [phase] program) phases


runAmp2 :: Program -> Int -> Program
runAmp2 program new_input =
  let program' = addInput new_input program
  in runProgram program'




-- -- REPL helper
-- load :: [Program]
-- load = do
--   content <- readFile "input.txt"
--   let program = loadProgram $ lines content
--   let amps = initAmps program [5..9]
--   return (program, amps)
