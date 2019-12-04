import Lib (calcFuel, sumFuel, calcFuel2)
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
    , sumFuelTest
    , calcFuel2Ex1
    , calcFuel2Ex2
    , calcFuel2Ex3]

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

calcFuel2Ex1 =
  testCase "calcFuel2 example 1" $ assertEqual [] 2 (calcFuel2 14)

calcFuel2Ex2 =
  testCase "calcFuel2 example 2" $ assertEqual [] 966 (calcFuel2 1969)

calcFuel2Ex3 =
  testCase "calcFuel2 example 3" $ assertEqual [] 50346 (calcFuel2 100756)


