import Lib
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ notDecreasingTest0
    , notDecreasingTest1
    , notDecreasingTest2
    , notDecreasingTest3
    , notDecreasingTest4
    , notDecreasingTest5 ]

notDecreasingTest0 = testCase "notDecreasing test 0" $ assertEqual [] True  (notDecreasing "")
notDecreasingTest1 = testCase "notDecreasing test 1" $ assertEqual [] True  (notDecreasing "000000")
notDecreasingTest2 = testCase "notDecreasing test 2" $ assertEqual [] True  (notDecreasing "012345")
notDecreasingTest3 = testCase "notDecreasing test 3" $ assertEqual [] False (notDecreasing "111110")
notDecreasingTest4 = testCase "notDecreasing test 4" $ assertEqual [] False (notDecreasing "10")
notDecreasingTest5 = testCase "notDecreasing test 5" $ assertEqual [] True (notDecreasing "1")

