import Day06
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import qualified Data.Map as Map

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [
      testGrowCell1
    ]

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")

testGrowCell1 = testCase "Test grow cell" $ assertEqual []
                ( Map.fromList [(Coordinate 41 290, 3),
                                (Coordinate 43 290, 3),
                                (Coordinate 42 289, 3),
                                (Coordinate 42 291, 3)],
                  Map.fromList [(Coordinate 41 290, 3),
                                (Coordinate 43 290, 3),
                                (Coordinate 42 289, 3),
                                (Coordinate 42 291, 3)])
                (growOneCell Map.empty Map.fromList [(Coordinate 42 290, 3)] 3)
                
