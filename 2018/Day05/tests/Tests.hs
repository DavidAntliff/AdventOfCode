import Day05
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [
      testReact1, testReact2, testReact3, testReact4, testReact5, testReact6, testReact7, testReact8
    , testStartReact1
    ]

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")

testReact1 = testCase "Test react" $ assertEqual [] "ABC" (react "ABC")

testReact2 = testCase "Test react" $ assertEqual [] "" (react "")

testReact3 = testCase "Test react" $ assertEqual [] "A" (react "A")

testReact4 = testCase "Test react" $ assertEqual [] "" (react "Aa")

testReact5 = testCase "Test react" $ assertEqual [] "AA" (react "AA")

testReact6 = testCase "Test react" $ assertEqual [] "" (react "aABb")

testReact7 = testCase "Test react" $ assertEqual [] "bb" (react "aAbbcC")

testReact8 = testCase "Test react" $ assertEqual [] "bB" (react "baAcCB")

testStartReact1 = testCase "Test start react" $ assertEqual [] "" (react "baAcCB")

