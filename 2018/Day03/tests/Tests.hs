import Day03 (Claim (Claim), sortClaims)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import qualified Data.Map as Map

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ testClaimEq1, testClaimEq2, testClaimEq3, testClaimEq4, testClaimEq5
    , testClaimOrd1, testClaimOrd2, testClaimOrd3
    , testSort1]

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")

testClaimEq1 = testCase "Test claim equality" $ assertBool [] (Claim 1 2 3 4 5 == Claim 8 2 3 4 5)
testClaimEq2 = testCase "Test claim equality" $ assertBool [] (Claim 1 8 3 4 5 /= Claim 1 2 3 4 5)
testClaimEq3 = testCase "Test claim equality" $ assertBool [] (Claim 1 2 8 4 5 /= Claim 1 2 3 4 5)
testClaimEq4 = testCase "Test claim equality" $ assertBool [] (Claim 1 2 3 8 5 /= Claim 1 2 3 4 5)
testClaimEq5 = testCase "Test claim equality" $ assertBool [] (Claim 1 2 3 4 8 /= Claim 1 2 3 4 5)

testClaimOrd1 = testCase "Test claim ordering" $ assertBool [] (Claim 1 0 0 1 1 < Claim 1 1 0 1 1)
testClaimOrd2 = testCase "Test claim ordering" $ assertBool [] (Claim 1 0 0 1 1 < Claim 1 0 1 1 1)
testClaimOrd3 = testCase "Test claim ordering" $ assertBool [] (Claim 1 1 0 1 1 < Claim 1 0 1 1 1)

testSort1 =
  testCase "Test sort claims 1" $ assertEqual []
    [Claim 1 1 1 1 1, Claim 19 3 3 4 2, Claim 17 4 3 8 10, Claim 21 3 4 12 1, Claim 23 4 4 1 1, Claim 15 0 5 6 6]
    (sortClaims [Claim 19 3 3 4 2, Claim 15 0 5 6 6, Claim 17 4 3 8 10, Claim 1 1 1 1 1, Claim 21 3 4 12 1, Claim 23 4 4 1 1])

