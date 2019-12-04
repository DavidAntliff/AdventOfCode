import Lib --(Program, runProgram)
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
    , replaceTest1
    , replaceTest2
    , replaceTest3
    , restoreGravityAssistTest
    ]


part1Ex1 =
  testCase "Part 1 Example Case 1" $ assertEqual []
  (Program {memory=[2, 0, 0, 0, 99], counter=4})
  (runProgram Program {memory=[1, 0, 0, 0, 99], counter=0})

part1Ex2 =
  testCase "Part 1 Example Case 2" $ assertEqual []
  (Program {memory=[2, 3, 0, 6, 99], counter=4})
  (runProgram Program {memory=[2, 3, 0, 3, 99], counter=0})

part1Ex3 =
  testCase "Part 1 Example Case 3" $ assertEqual []
  (Program {memory=[2, 4, 4, 5, 99, 9801], counter=4})
  (runProgram Program {memory=[2, 4, 4, 5, 99, 0], counter=0})

part1Ex4 =
  testCase "Part 1 Example Case 4" $ assertEqual []
  (Program {memory=[30, 1, 1, 4, 2, 5, 6, 0, 99], counter=8})
  (runProgram Program {memory=[1, 1, 1, 4, 99, 5, 6, 0, 99], counter=0})

readInputTest =
  testCase "readInput test" $ assertEqual []
  [1, 2, 321, 4, 55, 6, 7, 8]
  (readInput "1,2,321,4,55,6,7,8")

replaceTest1 =
  testCase "replace test 1" $ assertEqual []
  [1, 2, 3, 4, 5, 6, 7, 8]
  (replace 0 1 [0, 2, 3, 4, 5, 6, 7, 8])

replaceTest2 =
  testCase "replace test 2" $ assertEqual []
  [1, 2, 3, 4, 5, 6, 7, 8]
  (replace 3 4 [1, 2, 3, 0, 5, 6, 7, 8])

replaceTest3 =
  testCase "replace test 3" $ assertEqual []
  [1, 2, 3, 4, 5, 6, 7, 8]
  (replace 7 8 [1, 2, 3, 4, 5, 6, 7, (-42)])

restoreGravityAssistTest =
  testCase "restoreGravityAssist test" $ assertEqual []
  [1,12,2,3,1,1,2,3,1,3,4]
  (restoreGravityAssist [1,0,0,3,1,1,2,3,1,3,4])
