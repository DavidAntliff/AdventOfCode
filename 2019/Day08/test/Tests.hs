import Lib
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ countCharTest1
    , countCharTest2
    , countCharTest3 ]


countCharTest1 = testCase "countChar test 1" $ assertEqual [] 1 (countChar '0' "0")
countCharTest2 = testCase "countChar test 2" $ assertEqual [] 0 (countChar '0' "1")
countCharTest3 = testCase "countChar test 3" $ assertEqual [] 3 (countChar '1' "101010")
