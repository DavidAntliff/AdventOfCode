import Day05
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [
    ]

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")

