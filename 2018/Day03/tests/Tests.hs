import Day03 ()
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Data.Map as Map

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    []

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")
