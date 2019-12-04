import Lib (calcFuel, sumFuel)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [calcFuelTest
    , calcFuelEx1
    , calcFuelEx2
    , calcFuelEx3
    , calcFuelEx4
    , sumFuelTest]

calcFuelTest =
  testCase "calcFuel" $ assertEqual [] (-2) (calcFuel 0)

calcFuelEx1 =
  testCase "calcFuel example 1" $ assertEqual [] 2 (calcFuel 12)

calcFuelEx2 =
  testCase "calcFuel example 2" $ assertEqual [] 2 (calcFuel 14)

calcFuelEx3 =
  testCase "calcFuel example 3" $ assertEqual [] 654 (calcFuel 1969)

calcFuelEx4 =
  testCase "calcFuel example 4" $ assertEqual [] 33583 (calcFuel 100756)

sumFuelTest =
  testCase "sumFuel" $ assertEqual [] 34241 (sumFuel ["12","14","1969","100756"])

