import Day03 -- (sortClaims, parseClaim, addClaim)
import Claim
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import qualified Data.Map as Map
import qualified Data.Set as Set

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ testClaimEq1, testClaimEq2, testClaimEq3, testClaimEq4, testClaimEq5
    , testClaimOrd1, testClaimOrd2, testClaimOrd3
    , testSort1
    , testAddClaim1, testAddClaim2, testAddClaim3
    , testColumnView1, testColumnView2, testColumnView3, testColumnView4
    , testCombineViews1
    , testCountOverlaps1, testCountOverlaps2
    ]

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
  testCase "Test sort claims" $ assertEqual []
    [Claim 1 1 1 1 1, Claim 19 3 3 4 2, Claim 17 4 3 8 10, Claim 21 3 4 12 1, Claim 23 4 4 1 1, Claim 15 0 5 6 6]
    (sortClaims [Claim 19 3 3 4 2, Claim 15 0 5 6 6, Claim 17 4 3 8 10, Claim 1 1 1 1 1, Claim 21 3 4 12 1, Claim 23 4 4 1 1])

testParseClaim1 =
  testCase "Test parse claim" $ assertEqual [] (Right (Claim 4 6 7 3 2)) (parseClaim "#4 @ 6,7: 3x2")

testParseClaim2 =
  testCase "Test parse claim" $ assertEqual [] (Right (Claim 34 61 79 113 22)) (parseClaim "#34 @ 61,79: 113x22")


testAddClaim1 = testCase "Test add claim" $ assertEqual []
                (Map.fromList [(0, [1])])
                (addClaim Map.empty (\c -> x c) (\c -> w c) (Claim 1 0 0 1 1))

testAddClaim2 = testCase "Test add claim" $ assertEqual []
                (Map.fromList [(2, [1]), (3, [1]), (4, [1]), (5,[1])])
                (addClaim Map.empty (\c -> x c) (\c -> w c) (Claim 1 2 3 4 5))

testAddClaim3 = let init_map = Map.fromList [(0,[7]), (1,[7]), (5,[8,9]), (6,[8,10])]
                in testCase "Test add claim" $ assertEqual []
                   (Map.fromList [(0,[7]), (1,[7]),
                                  (3, [1]), (4, [1]), (5,[8,9,1]), (6,[8,10,1]),
                                  (7,[1])])
                   (addClaim init_map (\c -> x c) (\c -> w c) (Claim 1 3 2 5 4))

testColumnView1 = testCase "Test compute column view" $ assertEqual []
                  (Map.fromList [(0,[1]), (1,[1])])
                  (columnView [Claim 1 0 0 2 2])

testColumnView2 = testCase "Test compute column view" $ assertEqual []
                  (Map.fromList [(0,[1]), (1,[1]), (4,[2]), (5,[2])])
                  (columnView [Claim 1 0 0 2 2, Claim 2 4 0 2 2])

testColumnView3 = testCase "Test compute column view" $ assertEqual []
                  (Map.fromList [(0,[1]), (1,[1]), (2, [3]), (3, [3]), (4,[2]), (5,[2])])
                  (columnView [Claim 1 0 0 2 2, Claim 2 4 0 2 2, Claim 3 2 2 2 2])

testColumnView4 = testCase "Test compute column view" $ assertEqual []
                  (Map.fromList [(0,[1]), (1,[1,4]), (2, [3,4]), (3, [3,4]), (4,[2,4]), (5,[2])])
                  (columnView [Claim 1 0 0 2 2, Claim 2 4 0 2 2, Claim 3 2 2 2 2, Claim 4 1 5 4 2])

testCombineViews1 = let claims = [Claim 1 2 2 2 2]
                        row_view = rowView claims
                        col_view = columnView claims
                    in testCase "Test combine views" $ assertEqual []
                       [((2,2), [1]), ((2,3), [1]), ((3,2), [1]), ((3,3), [1])]
                       (combineViews col_view row_view)

testCountOverlaps1 = let claims = ["#1 @ 0,0: 2x2", "#2 @ 2,2: 2x2"]
                     in testCase "Test count overlaps" $ assertEqual []
                        (0, Set.fromList [1, 2])
                        (countOverlaps claims)

testCountOverlaps2 = let claims = ["#1 @ 0,0: 2x2", "#2 @ 1,1: 2x2"]
                     in testCase "Test count overlaps" $ assertEqual []
                        (1, Set.fromList [])
                        (countOverlaps claims)
