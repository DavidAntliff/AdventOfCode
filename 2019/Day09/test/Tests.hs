import Lib --(Program, runProgram)
import qualified Data.Sequence as Seq

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ part1Ex1
    , part1Ex2
    , part1Ex3
    , part1Ex4
    , readMemoryTest
    , inputTest1
    , inputTest2
    , outputTest1
    , outputTest2
    , inputThen4Test
    , loadPositionTest
    , loadImmediateTest
    , opcodeModeTest1
    , decodeParamModesTest1
    , decodeParamModesTest2
    , loadParamTest1
    , loadParamTest2
    , outputImmediateTest
    , outputPositionTest
    , jumpIfTrueInstructionPosPosFalseTest
    , jumpIfTrueInstructionPosPosTrueTest
    , jumpIfTrueInstructionImmImmFalseTest
    , jumpIfTrueInstructionImmImmTrueTest
    , jumpIfFalseInstructionPosPosFalseTest
    , jumpIfFalseInstructionPosPosTrueTest
    , jumpIfFalseInstructionImmImmFalseTest
    , jumpIfFalseInstructionImmImmTrueTest
    , lessThanInstructionPosPosFalseTest
    , lessThanInstructionPosPosTrueTest
    , equalsInstructionPosImmFalseTest
    , equalsInstructionImmPosTrueTest
    , part2Example1EqualTest
    , part2Example1NotEqualTest
    , part2Example2LessThanTest
    , part2Example2NotLessThanTest
    , part2Example3EqualTest
    , part2Example3NotEqualTest
    , part2Example4LessThanTest
    , part2Example4NotLessThanTest
    , part2Example5JumpFalseTest
    , part2Example5JumpTrueTest
    , part2Example6JumpFalseTest
    , part2Example6JumpTrueTest
    , blockOnEmptyInputTest
    , addInputClearsBlockTest
    , setInputClearsBlockTest
    , extendMemoryTest1
    , extendMemoryTest2
    , relBaseInitTest
    , relBaseIncTest
    , relBaseDecTest
    , loadRelativeTest1
    , loadRelativeTest2
    , relAddInstructionTest1
    , relAddInstructionTest2
    , relMultInstructionTest
    , relInputInstructionTest
    , relLessThanInstructionTest
    , relEqualsInstructionTest
    , quineTest
    ]


part1Ex1 =
  testCase "Part 1 Example Case 1" $ assertEqual []
  (Seq.fromList [2, 0, 0, 0, 99], 4, 0, [], [], Terminated)
  (runProgram (Seq.fromList [1, 0, 0, 0, 99], 0, 0, [], [], Running))

part1Ex2 =
  testCase "Part 1 Example Case 2" $ assertEqual []
  (Seq.fromList [2, 3, 0, 6, 99], 4, 0, [], [], Terminated)
  (runProgram (Seq.fromList [2, 3, 0, 3, 99], 0, 0, [], [], Running))

part1Ex3 =
  testCase "Part 1 Example Case 3" $ assertEqual []
  (Seq.fromList [2, 4, 4, 5, 99, 9801], 4, 0, [], [], Terminated)
  (runProgram (Seq.fromList [2, 4, 4, 5, 99, 0], 0, 0, [], [], Running))

part1Ex4 =
  testCase "Part 1 Example Case 4" $ assertEqual []
  (Seq.fromList [30, 1, 1, 4, 2, 5, 6, 0, 99], 8, 0, [], [], Terminated)
  (runProgram (Seq.fromList [1, 1, 1, 4, 99, 5, 6, 0, 99], 0, 0, [], [], Running))

readMemoryTest =
  testCase "readMemory test" $ assertEqual []
  (Seq.fromList [1, 2, 321, 4, 55, 6, 7, 8])
  (readMemory "1,2,321,4,55,6,7,8")

inputTest1 =
  testCase "opcode 3 test 1" $ assertEqual []
  (Seq.fromList [3, 3, 99, 42], 2, 0, [], [], Terminated)
  (runProgram (Seq.fromList [3, 3, 99, 0], 0, 0, [42], [], Running))

inputTest2 =
  testCase "opcode 3 test 2" $ assertEqual []
  (Seq.fromList [3, 5, 3, 6, 99, 14, 79], 4, 0, [], [], Terminated)
  (runProgram (Seq.fromList [3, 5, 3, 6, 99, 0, 0], 0, 0, [14, 79], [], Running))

outputTest1 =
  testCase "opcode 4 test 1" $ assertEqual []
  (Seq.fromList [4, 3, 99, 42], 2, 0, [], [42], Terminated)
  (runProgram (Seq.fromList [4, 3, 99, 42], 0, 0, [], [], Running))

outputTest2 =
  testCase "opcode 4 test 2" $ assertEqual []
  (Seq.fromList [4, 5, 4, 6, 99, 19, 43], 4, 0, [], [43, 19], Terminated)
  (runProgram (Seq.fromList [4, 5, 4, 6, 99, 19, 43], 0, 0, [], [], Running))

inputThen4Test =
  testCase "opcode 4 test 2" $ assertEqual []
  (Seq.fromList [3, 5, 4, 5, 99, 42], 4, 0, [], [42], Terminated)
  (runProgram (Seq.fromList [3, 5, 4, 5, 99, 0], 0, 0, [42], [], Running))

loadPositionTest =
  testCase "loadPosition test" $ assertEqual []
  (2)
  (loadPosition (Seq.fromList [1,2,3,4]) 0)

loadImmediateTest =
  testCase "loadImmediate test" $ assertEqual []
  (1)
  (loadImmediate (Seq.fromList [1,2,3,4]) 0)

opcodeModeTest1 =
  testCase "opcodeMode test" $ assertEqual []
  (Seq.fromList [1002, 4, 3, 4, 99], 4, 0, [], [], Terminated)
  (runProgram (Seq.fromList [1002, 4, 3, 4, 33], 0, 0, [], [], Running))

decodeParamModesTest1 =
  testCase "decodeParamMode test 1" $ assertEqual []
  ([3, 2, 1])
  (decodeParamModes 12345)

decodeParamModesTest2 =
  testCase "decodeParamMode test 2" $ assertEqual []
  ([0, 1, 0])
  (decodeParamModes 1002)

loadParamTest1 =
  testCase "loadParam test 1" $ assertEqual []
  2
  (loadParam (Seq.fromList [3,2,1,0]) 2 0 0)

loadParamTest2 =
  testCase "loadParam test 2" $ assertEqual []
  1
  (loadParam (Seq.fromList [3,2,1,0]) 2 0 1)

outputImmediateTest =
  testCase "output immediate test" $ assertEqual []
  (Seq.fromList [104, 0, 99], 2, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [104, 0, 99], 0, 0, [], [], Running))

outputPositionTest =
  testCase "output position test" $ assertEqual []
  (Seq.fromList [4, 0, 99], 2, 0, [], [4], Terminated)
  (runProgram (Seq.fromList [4, 0, 99], 0, 0, [], [], Running))

jumpIfTrueInstructionPosPosFalseTest =
  testCase "jumpIfTrue pos pos false test" $ assertEqual []
  (Seq.fromList [5, 2, 0], 3, 0, [], [], Running)
  (jumpIfTrueInstruction (Seq.fromList [5, 2, 0], 0, 0, [], [], Running))

jumpIfTrueInstructionPosPosTrueTest =
  testCase "jumpIfTrue pos pos true test" $ assertEqual []
  (Seq.fromList [5, 2, 1], 2, 0, [], [], Running)
  (jumpIfTrueInstruction (Seq.fromList [5, 2, 1], 0, 0, [], [], Running))

jumpIfTrueInstructionImmImmFalseTest =
  testCase "jumpIfTrue imm imm false test" $ assertEqual []
  (Seq.fromList [1105, 0, 0], 3, 0, [], [], Running)
  (jumpIfTrueInstruction (Seq.fromList [1105, 0, 0], 0, 0, [], [], Running))

jumpIfTrueInstructionImmImmTrueTest =
  testCase "jumpIfTrue imm imm true test" $ assertEqual []
  (Seq.fromList [1105, 1, 1], 1, 0, [], [], Running)
  (jumpIfTrueInstruction (Seq.fromList [1105, 1, 1], 0, 0, [], [], Running))

jumpIfFalseInstructionPosPosFalseTest =
  testCase "jumpIfFalse pos pos false test" $ assertEqual []
  (Seq.fromList [6, 2, 0], 6, 0, [], [], Running)
  (jumpIfFalseInstruction (Seq.fromList [6, 2, 0], 0, 0, [], [], Running))

jumpIfFalseInstructionPosPosTrueTest =
  testCase "jumpIfFalse pos pos true test" $ assertEqual []
  (Seq.fromList [6, 2, 1], 3, 0, [], [], Running)
  (jumpIfFalseInstruction (Seq.fromList [6, 2, 1], 0, 0, [], [], Running))

jumpIfFalseInstructionImmImmFalseTest =
  testCase "jumpIfFalse imm imm false test" $ assertEqual []
  (Seq.fromList [1106, 0, 0], 0, 0, [], [], Running)
  (jumpIfFalseInstruction (Seq.fromList [1106, 0, 0], 0, 0, [], [], Running))

jumpIfFalseInstructionImmImmTrueTest =
  testCase "jumpIfFalse imm imm true test" $ assertEqual []
  (Seq.fromList [1106, 1, 1], 3, 0, [], [], Running)
  (jumpIfFalseInstruction (Seq.fromList [1106, 1, 1], 0, 0, [], [], Running))

lessThanInstructionPosPosFalseTest =
  testCase "lessThanInstruction false test" $ assertEqual []
  (Seq.fromList [7, 0, 1, 4, 0], 4, 0, [], [], Running)
  (lessThanInstruction (Seq.fromList [7, 0, 1, 4, (-1)], 0, 0, [], [], Running))

lessThanInstructionPosPosTrueTest =
  testCase "lessThanInstruction true test" $ assertEqual []
  (Seq.fromList [7, 1, 0, 4, 1], 4, 0, [], [], Running)
  (lessThanInstruction (Seq.fromList [7, 1, 0, 4, (-1)], 0, 0, [], [], Running))

equalsInstructionPosImmFalseTest =
  testCase "equalsInstruction false test" $ assertEqual []
  (Seq.fromList [1008, 0, 1, 4, 0], 4, 0, [], [], Running)
  (equalsInstruction (Seq.fromList [1008, 0, 1, 4, (-1)], 0, 0, [], [], Running))

equalsInstructionImmPosTrueTest =
  testCase "equalsInstruction true test" $ assertEqual []
  (Seq.fromList [108, (-1), 4, 4, 1], 4, 0, [], [], Running)
  (equalsInstruction (Seq.fromList [108, (-1), 4, 4, (-1)], 0, 0, [], [], Running))

part2Example1EqualTest =
  testCase "part2 example 1 equal test" $ assertEqual []
  (Seq.fromList [3,9,8,9,10,9,4,9,99,1,8], 8, 0, [], [1], Terminated)
  (runProgram (Seq.fromList [3,9,8,9,10,9,4,9,99,(-1),8], 0, 0, [8], [], Running))

part2Example1NotEqualTest =
  testCase "part2 example 1 not equal test" $ assertEqual []
  (Seq.fromList [3,9,8,9,10,9,4,9,99,0,8], 8, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [3,9,8,9,10,9,4,9,99,(-1),8], 0, 0, [7], [], Running))

part2Example2LessThanTest =
  testCase "part2 example 2 less than test" $ assertEqual []
  (Seq.fromList [3,9,7,9,10,9,4,9,99,1,8], 8, 0, [], [1], Terminated)
  (runProgram (Seq.fromList [3,9,7,9,10,9,4,9,99,(-1),8], 0, 0, [7], [], Running))

part2Example2NotLessThanTest =
  testCase "part2 example 2 not less than test" $ assertEqual []
  (Seq.fromList [3,9,7,9,10,9,4,9,99,0,8], 8, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [3,9,7,9,10,9,4,9,99,(-1),8], 0, 0, [9], [], Running))

part2Example3EqualTest =
  testCase "part2 example 3 equal test" $ assertEqual []
  (Seq.fromList [3,3,1108,1,8,3,4,3,99], 8, 0, [], [1], Terminated)
  (runProgram (Seq.fromList [3,3,1108,(-1),8,3,4,3,99], 0, 0, [8], [], Running))

part2Example3NotEqualTest =
  testCase "part2 example 3 not equal test" $ assertEqual []
  (Seq.fromList [3,3,1108,0,8,3,4,3,99], 8, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [3,3,1108,(-1),8,3,4,3,99], 0, 0, [7], [], Running))

part2Example4LessThanTest =
  testCase "part2 example 4 less than test" $ assertEqual []
  (Seq.fromList [3,3,1107,1,8,3,4,3,99], 8, 0, [], [1], Terminated)
  (runProgram (Seq.fromList [3,3,1107,-1,8,3,4,3,99], 0, 0, [7], [], Running))

part2Example4NotLessThanTest =
  testCase "part2 example 4 not less than test" $ assertEqual []
  (Seq.fromList [3,3,1107,0,8,3,4,3,99], 8, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [3,3,1107,(-1),8,3,4,3,99], 0, 0, [9], [], Running))

part2Example5JumpFalseTest =
  testCase "part2 example 5 jump false test" $ assertEqual []
  (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9], 11, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,(-1),0,1,9], 0, 0, [0], [], Running))

part2Example5JumpTrueTest =
  testCase "part2 example 5 jump true test" $ assertEqual []
  (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,44,1,1,9], 11, 0, [], [1], Terminated)
  (runProgram (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,(-1),0,1,9], 0, 0, [44], [], Running))

part2Example6JumpFalseTest =
  testCase "part2 example 6 jump false test" $ assertEqual []
  (Seq.fromList [3,3,1105,0,9,1101,0,0,12,4,12,99,0], 11, 0, [], [0], Terminated)
  (runProgram (Seq.fromList [3,3,1105,(-1),9,1101,0,0,12,4,12,99,1], 0, 0, [0], [], Running))

part2Example6JumpTrueTest =
  testCase "part2 example 6 jump true test" $ assertEqual []
  (Seq.fromList [3,3,1105,44,9,1101,0,0,12,4,12,99,1], 11, 0, [], [1], Terminated)
  (runProgram (Seq.fromList [3,3,1105,(-1),9,1101,0,0,12,4,12,99,1], 0, 0, [44], [], Running))

blockOnEmptyInputTest =
  testCase "block on empty input test" $ assertEqual []
  (Seq.fromList [3,0], 0, 0, [], [], Blocked)
  (runProgram (Seq.fromList [3,0], 0, 0, [], [], Running))

addInputClearsBlockTest =
  testCase "addInput clears block test" $ assertEqual []
  (Seq.fromList [3,2,0], 0, 0, [42], [], Running)
  (addInput 42 $ (Seq.fromList [3,2,0], 0, 0, [], [], Blocked))

setInputClearsBlockTest =
  testCase "setInput clears block test" $ assertEqual []
  (Seq.fromList [3,2,0], 0, 0, [42, 19, 17], [], Running)
  (setInput [42, 19, 17] $ (Seq.fromList [3,2,0], 0, 0, [], [], Blocked))

-- Day 09

extendMemoryTest1 =
  testCase "extendMemory test 1" $ assertEqual []
  (Seq.fromList [3,2,0,0,0,0,0,0], 0, 0, [], [], Running)
  (extendMemory 8 (Seq.fromList [3,2,0], 0, 0, [], [], Running))

extendMemoryTest2 =
  testCase "extendMemory test 2" $ assertEqual []
  (Seq.fromList [3,2,0,0,1], 0, 0, [], [], Running)
  (extendMemory 3 (Seq.fromList [3,2,0,0,1], 0, 0, [], [], Running))

relBaseInitTest =
  testCase "relbase initialised to zero test" $ assertEqual []
  (0)
  (getRelBase $ loadProgram "3,2,0")

relBaseIncTest =
  testCase "relbase increases test" $ assertEqual []
  (63)
  (getRelBase $ adjustRelbaseInstruction (Seq.fromList [109, 42, 99], 0, 21, [], [], Running))

relBaseDecTest =
  testCase "relbase decreases test" $ assertEqual []
  (-9400)
  (getRelBase $ adjustRelbaseInstruction (Seq.fromList [9, 3, 99, (-9456)], 0, 56, [], [], Running))

loadRelativeTest1 =
  testCase "loadRelative test 1" $ assertEqual []
  (2)
  (loadRelative (Seq.fromList [1,2,3,4]) 0 0)

loadRelativeTest2 =
  testCase "loadRelative test 2" $ assertEqual []
  (4)
  (loadRelative (Seq.fromList [1,2,3,4]) 1 1)

relAddInstructionTest1 =
  testCase "addInstruction with relative mode test 1" $ assertEqual []
  (Seq.fromList [2201, 2, 3, 7, 99, 5, 6, 11], 4, 3, [], [], Running)
  (addInstruction (Seq.fromList [2201, 2, 3, 7, 99, 5, 6, 0], 0, 3, [], [], Running))

relAddInstructionTest2 =
  testCase "addInstruction with relative mode test 2 " $ assertEqual []
  (Seq.fromList [22201, 2, 3, 7, 99, 5, 6, 0, 0, 0, 11], 4, 3, [], [], Running)
  (addInstruction (Seq.fromList [22201, 2, 3, 7, 99, 5, 6, 0, 0, 0, 0], 0, 3, [], [], Running))

relMultInstructionTest =
  testCase "multInstruction with relative mode test" $ assertEqual []
  (Seq.fromList [21102, 2, 3, 5, 99, 6], 4, 0, [], [], Running)
  (multInstruction (Seq.fromList [21102, 2, 3, 5, 99, 0], 0, 0, [], [], Running))

relInputInstructionTest =
  testCase "inputInstruction with relative mode test" $ assertEqual []
  (Seq.fromList [203, (-3), 17], 2, 5, [], [], Running)
  (inputInstruction (Seq.fromList [203, (-3), 0], 0, 5, [17], [], Running))

relLessThanInstructionTest =
  testCase "lessThanInstruction with relative mode true test" $ assertEqual []
  (Seq.fromList [21107, 0, 1, 1, 1], 4, 3, [], [], Running)
  (lessThanInstruction (Seq.fromList [21107, 0, 1, 1, (-1)], 0, 3, [], [], Running))

relEqualsInstructionTest =
  testCase "equalsInstruction with relative mode true test" $ assertEqual []
  (Seq.fromList [1208, 7, 1, 4, 1], 4, (-5), [], [], Running)
  (equalsInstruction (Seq.fromList [1208, 7, 1, 4, (-1)], 0, (-5), [], [], Running))

quineTest =
  testCase "replicate test program" $
  let input = [109,1,204,(-1),1001,100,1,100,1008,100,16,101,1006,101,0,99]
  in assertEqual []
     (reverse input)
     (getOutput $ runProgram $ extendMemory 1000 $ (Seq.fromList input, 0, 0, [], [], Running))

