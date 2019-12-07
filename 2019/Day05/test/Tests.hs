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
