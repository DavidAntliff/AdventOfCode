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
           (mkGraph [(0,'A'),(1,'B'),(2,'C'),(3,'D'),(4,'E'),(5,'F')]
           [(0,1,()),(0,3,()),(1,4,()),(2,0,()),(2,5,()),(3,4,()),(5,4,())])
           genTestGraph

testFindStartingNodes1 = testCase "Test findStartingNodes" $ assertEqual []
           [2]
           (findStartingNodes genTestGraph)

testKahnAlgorithm1 = testCase "Test kahnAlgorithm" $ assertEqual []
           [2,0,1,3,5,4]
           (kahnAlgorithm genTestGraph [2] [])
