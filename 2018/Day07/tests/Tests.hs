import Day07
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Data.Graph.Inductive.Graph (mkGraph)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [
      testFgl1
    , testFindStartingNodes1
    , testKahnAlgorithm1
    ]

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")


testFgl1 = testCase "Test FGL" $ assertEqual []
           (mkGraph [(1,'A'),(2,'B'),(3,'C'),(4,'D'),(5,'E'),(6,'F')] [(1,2,()),(1,4,()),(2,5,()),(3,1,()),(3,6,()),(4,5,()),(6,5,())])
           genTestGraph

testFindStartingNodes1 = testCase "Test findStartingNodes" $ assertEqual []
           [3]
           (findStartingNodes genTestGraph)

testKahnAlgorithm1 = testCase "Test kahnAlgorithm" $ assertEqual []
           [3,1,2,4,6,5]
           (kahnAlgorithm genTestGraph [3])
