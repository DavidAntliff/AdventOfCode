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
    , opcode3Test1
    , opcode3Test2
    , opcode4Test1
    , opcode4Test2
    , opcode3then4Test
    , loadPositionTest
    , loadImmediateTest
    , opcodeModeTest1
    , decodeParamModesTest1
    , decodeParamModesTest2
    , loadParamTest1
    , loadParamTest2
    , opcode4ImmediateTest
    , opcode4PositionTest
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
    ]


part1Ex1 =
  testCase "Part 1 Example Case 1" $ assertEqual []
  (Seq.fromList [2, 0, 0, 0, 99], 4, [], [])
  (runProgram (Seq.fromList [1, 0, 0, 0, 99], 0, [], []))

part1Ex2 =
  testCase "Part 1 Example Case 2" $ assertEqual []
  (Seq.fromList [2, 3, 0, 6, 99], 4, [], [])
  (runProgram (Seq.fromList [2, 3, 0, 3, 99], 0, [], []))

part1Ex3 =
  testCase "Part 1 Example Case 3" $ assertEqual []
  (Seq.fromList [2, 4, 4, 5, 99, 9801], 4, [], [])
  (runProgram (Seq.fromList [2, 4, 4, 5, 99, 0], 0, [], []))

part1Ex4 =
  testCase "Part 1 Example Case 4" $ assertEqual []
  (Seq.fromList [30, 1, 1, 4, 2, 5, 6, 0, 99], 8, [], [])
  (runProgram (Seq.fromList [1, 1, 1, 4, 99, 5, 6, 0, 99], 0, [], []))

readMemoryTest =
  testCase "readMemory test" $ assertEqual []
  (Seq.fromList [1, 2, 321, 4, 55, 6, 7, 8])
  (readMemory "1,2,321,4,55,6,7,8")

opcode3Test1 =
  testCase "opcode 3 test 1" $ assertEqual []
  (Seq.fromList [3, 3, 99, 42], 2, [], [])
  (runProgram (Seq.fromList [3, 3, 99, 0], 0, [42], []))

opcode3Test2 =
  testCase "opcode 3 test 2" $ assertEqual []
  (Seq.fromList [3, 5, 3, 6, 99, 14, 79], 4, [], [])
  (runProgram (Seq.fromList [3, 5, 3, 6, 99, 0, 0], 0, [14, 79], []))

opcode4Test1 =
  testCase "opcode 4 test 1" $ assertEqual []
  (Seq.fromList [4, 3, 99, 42], 2, [], [42])
  (runProgram (Seq.fromList [4, 3, 99, 42], 0, [], []))

opcode4Test2 =
  testCase "opcode 4 test 2" $ assertEqual []
  (Seq.fromList [4, 5, 4, 6, 99, 19, 43], 4, [], [43, 19])
  (runProgram (Seq.fromList [4, 5, 4, 6, 99, 19, 43], 0, [], []))

opcode3then4Test =
  testCase "opcode 4 test 2" $ assertEqual []
  (Seq.fromList [3, 5, 4, 5, 99, 42], 4, [], [42])
  (runProgram (Seq.fromList [3, 5, 4, 5, 99, 0], 0, [42], []))

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
  (Seq.fromList [1002, 4, 3, 4, 99], 4, [], [])
  (runProgram (Seq.fromList [1002, 4, 3, 4, 33], 0, [], []))

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
  (loadParam (Seq.fromList [3,2,1,0]) 2 0)

loadParamTest2 =
  testCase "loadParam test 2" $ assertEqual []
  1
  (loadParam (Seq.fromList [3,2,1,0]) 2 1)

opcode4ImmediateTest =
  testCase "opcode4 immediate test" $ assertEqual []
  (Seq.fromList [104, 0, 99], 2, [], [0])
  (runProgram (Seq.fromList [104, 0, 99], 0, [], []))

opcode4PositionTest =
  testCase "opcode4 position test" $ assertEqual []
  (Seq.fromList [4, 0, 99], 2, [], [4])
  (runProgram (Seq.fromList [4, 0, 99], 0, [], []))

jumpIfTrueInstructionPosPosFalseTest =
  testCase "jumpIfTrue pos pos false test" $ assertEqual []
  (Seq.fromList [5, 2, 0], 3, [], [])
  (jumpIfTrueInstruction (Seq.fromList [5, 2, 0], 0, [], []))

jumpIfTrueInstructionPosPosTrueTest =
  testCase "jumpIfTrue pos pos true test" $ assertEqual []
  (Seq.fromList [5, 2, 1], 2, [], [])
  (jumpIfTrueInstruction (Seq.fromList [5, 2, 1], 0, [], []))

jumpIfTrueInstructionImmImmFalseTest =
  testCase "jumpIfTrue imm imm false test" $ assertEqual []
  (Seq.fromList [1105, 0, 0], 3, [], [])
  (jumpIfTrueInstruction (Seq.fromList [1105, 0, 0], 0, [], []))

jumpIfTrueInstructionImmImmTrueTest =
  testCase "jumpIfTrue imm imm true test" $ assertEqual []
  (Seq.fromList [1105, 1, 1], 1, [], [])
  (jumpIfTrueInstruction (Seq.fromList [1105, 1, 1], 0, [], []))

jumpIfFalseInstructionPosPosFalseTest =
  testCase "jumpIfFalse pos pos false test" $ assertEqual []
  (Seq.fromList [6, 2, 0], 6, [], [])
  (jumpIfFalseInstruction (Seq.fromList [6, 2, 0], 0, [], []))

jumpIfFalseInstructionPosPosTrueTest =
  testCase "jumpIfFalse pos pos true test" $ assertEqual []
  (Seq.fromList [6, 2, 1], 3, [], [])
  (jumpIfFalseInstruction (Seq.fromList [6, 2, 1], 0, [], []))

jumpIfFalseInstructionImmImmFalseTest =
  testCase "jumpIfFalse imm imm false test" $ assertEqual []
  (Seq.fromList [1106, 0, 0], 0, [], [])
  (jumpIfFalseInstruction (Seq.fromList [1106, 0, 0], 0, [], []))

jumpIfFalseInstructionImmImmTrueTest =
  testCase "jumpIfFalse imm imm true test" $ assertEqual []
  (Seq.fromList [1106, 1, 1], 3, [], [])
  (jumpIfFalseInstruction (Seq.fromList [1106, 1, 1], 0, [], []))

lessThanInstructionPosPosFalseTest =
  testCase "lessThanInstruction false test" $ assertEqual []
  (Seq.fromList [7, 0, 1, 4, 0], 4, [], [])
  (lessThanInstruction (Seq.fromList [7, 0, 1, 4, (-1)], 0, [], []))

lessThanInstructionPosPosTrueTest =
  testCase "lessThanInstruction true test" $ assertEqual []
  (Seq.fromList [7, 1, 0, 4, 1], 4, [], [])
  (lessThanInstruction (Seq.fromList [7, 1, 0, 4, (-1)], 0, [], []))

equalsInstructionPosImmFalseTest =
  testCase "equalsInstruction false test" $ assertEqual []
  (Seq.fromList [1008, 0, 1, 4, 0], 4, [], [])
  (equalsInstruction (Seq.fromList [1008, 0, 1, 4, (-1)], 0, [], []))

equalsInstructionImmPosTrueTest =
  testCase "equalsInstruction true test" $ assertEqual []
  (Seq.fromList [108, (-1), 4, 4, 1], 4, [], [])
  (equalsInstruction (Seq.fromList [108, (-1), 4, 4, (-1)], 0, [], []))

part2Example1EqualTest =
  testCase "part2 example 1 equal test" $ assertEqual []
  (Seq.fromList [3,9,8,9,10,9,4,9,99,1,8], 8, [], [1])
  (runProgram (Seq.fromList [3,9,8,9,10,9,4,9,99,(-1),8], 0, [8], []))

part2Example1NotEqualTest =
  testCase "part2 example 1 not equal test" $ assertEqual []
  (Seq.fromList [3,9,8,9,10,9,4,9,99,0,8], 8, [], [0])
  (runProgram (Seq.fromList [3,9,8,9,10,9,4,9,99,(-1),8], 0, [7], []))

part2Example2LessThanTest =
  testCase "part2 example 2 less than test" $ assertEqual []
  (Seq.fromList [3,9,7,9,10,9,4,9,99,1,8], 8, [], [1])
  (runProgram (Seq.fromList [3,9,7,9,10,9,4,9,99,(-1),8], 0, [7], []))

part2Example2NotLessThanTest =
  testCase "part2 example 2 not less than test" $ assertEqual []
  (Seq.fromList [3,9,7,9,10,9,4,9,99,0,8], 8, [], [0])
  (runProgram (Seq.fromList [3,9,7,9,10,9,4,9,99,(-1),8], 0, [9], []))

part2Example3EqualTest =
  testCase "part2 example 3 equal test" $ assertEqual []
  (Seq.fromList [3,3,1108,1,8,3,4,3,99], 8, [], [1])
  (runProgram (Seq.fromList [3,3,1108,(-1),8,3,4,3,99], 0, [8], []))

part2Example3NotEqualTest =
  testCase "part2 example 3 not equal test" $ assertEqual []
  (Seq.fromList [3,3,1108,0,8,3,4,3,99], 8, [], [0])
  (runProgram (Seq.fromList [3,3,1108,(-1),8,3,4,3,99], 0, [7], []))

part2Example4LessThanTest =
  testCase "part2 example 4 less than test" $ assertEqual []
  (Seq.fromList [3,3,1107,1,8,3,4,3,99], 8, [], [1])
  (runProgram (Seq.fromList [3,3,1107,-1,8,3,4,3,99], 0, [7], []))

part2Example4NotLessThanTest =
  testCase "part2 example 4 not less than test" $ assertEqual []
  (Seq.fromList [3,3,1107,0,8,3,4,3,99], 8, [], [0])
  (runProgram (Seq.fromList [3,3,1107,(-1),8,3,4,3,99], 0, [9], []))

part2Example5JumpFalseTest =
  testCase "part2 example 5 jump false test" $ assertEqual []
  (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9], 11, [], [0])
  (runProgram (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,(-1),0,1,9], 0, [0], []))

part2Example5JumpTrueTest =
  testCase "part2 example 5 jump true test" $ assertEqual []
  (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,44,1,1,9], 11, [], [1])
  (runProgram (Seq.fromList [3,12,6,12,15,1,13,14,13,4,13,99,(-1),0,1,9], 0, [44], []))

part2Example6JumpFalseTest =
  testCase "part2 example 6 jump false test" $ assertEqual []
  (Seq.fromList [3,3,1105,0,9,1101,0,0,12,4,12,99,0], 11, [], [0])
  (runProgram (Seq.fromList [3,3,1105,(-1),9,1101,0,0,12,4,12,99,1], 0, [0], []))

part2Example6JumpTrueTest =
  testCase "part2 example 6 jump true test" $ assertEqual []
  (Seq.fromList [3,3,1105,44,9,1101,0,0,12,4,12,99,1], 11, [], [1])
  (runProgram (Seq.fromList [3,3,1105,(-1),9,1101,0,0,12,4,12,99,1], 0, [44], []))


-- Day 07
