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
    , readInputTest
    ]


part1Ex1 =
  testCase "Part 1 Example Case 1" $ assertEqual []
  (Seq.fromList [2, 0, 0, 0, 99], 4)
  (runProgram (Seq.fromList [1, 0, 0, 0, 99], 0))

part1Ex2 =
  testCase "Part 1 Example Case 2" $ assertEqual []
  (Seq.fromList [2, 3, 0, 6, 99], 4)
  (runProgram (Seq.fromList [2, 3, 0, 3, 99], 0))

part1Ex3 =
  testCase "Part 1 Example Case 3" $ assertEqual []
  (Seq.fromList [2, 4, 4, 5, 99, 9801], 4)
  (runProgram (Seq.fromList [2, 4, 4, 5, 99, 0], 0))

part1Ex4 =
  testCase "Part 1 Example Case 4" $ assertEqual []
  (Seq.fromList [30, 1, 1, 4, 2, 5, 6, 0, 99], 8)
  (runProgram (Seq.fromList [1, 1, 1, 4, 99, 5, 6, 0, 99], 0))

readInputTest =
  testCase "readInput test" $ assertEqual []
  (Seq.fromList [1, 2, 321, 4, 55, 6, 7, 8])
  (readInput "1,2,321,4,55,6,7,8")
