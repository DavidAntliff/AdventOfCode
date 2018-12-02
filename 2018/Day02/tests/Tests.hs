import Day02 (getCounts, record, histogram)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Data.Map as Map

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [record1, record2, record3,
     histogram1, histogram2,
     ex1, ex2, ex3, ex4, ex5, ex6, ex7]

-- Part 1

record1 =
  testCase "Use a map to count" $ assertEqual [] (Map.fromList [('a',1)]) (record Map.empty 'a')

record2 =
  testCase "Use a map to count" $ assertEqual [] (Map.fromList [('a',1),('b',1)]) (record (record Map.empty 'a') 'b')

record3 =
  testCase "Use a map to count" $ assertEqual [] (Map.fromList [('a',1),('b',2)]) (record (record (record Map.empty 'b') 'a') 'b')

histogram1 =
  testCase "String histogram" $ assertEqual [] (Map.fromList [('a',1), ('b',1), ('c', 1)]) (histogram "abc")

histogram2 =
  testCase "String histogram" $ assertEqual [] (Map.fromList [('a',2), ('b',3), ('c', 1)]) (histogram "bababc")

ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")
ex2 = testCase "Part 1 Example Case 1" $ assertEqual [] (1, 1) (getCounts "bababc")
ex3 = testCase "Part 1 Example Case 1" $ assertEqual [] (1, 0) (getCounts "abbcde")
ex4 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 1) (getCounts "abcccd")
ex5 = testCase "Part 1 Example Case 1" $ assertEqual [] (1, 0) (getCounts "aabcdd")
ex6 = testCase "Part 1 Example Case 1" $ assertEqual [] (1, 0) (getCounts "abcdee")
ex7 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 1) (getCounts "ababab")

-- Part 2

