import Day01 (calibrate, stripPlus)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [doStripPlus, dontStripMinus, example1, example2, example3]

doStripPlus =
  testCase "Strip + from string with leading +" $ assertEqual [] "123" (stripPlus "+123")

dontStripMinus =
  testCase "Don't strip - from string with leading -" $ assertEqual [] "-123" (stripPlus "-123")

example1 =
  testCase "Example Case 1" $ assertEqual [] 3 (calibrate [ "+1", "+1", "+1" ])

example2 =
  testCase "Example Case 2" $ assertEqual [] 0 (calibrate [ "+1", "+1", "-2" ])

example3 =
  testCase "Example Case 3" $ assertEqual [] (-6) (calibrate [ "-1", "-2", "-3" ])
