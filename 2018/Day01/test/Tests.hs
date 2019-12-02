import Lib (calibrate, stripPlus, findFirstDuplicate)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [doStripPlus, dontStripMinus,
     example1, example2, example3,
     example4, example5, example6, example7]

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

-- Part 2

example4 =
  testCase "Example Case 4" $ assertEqual [] 0 (findFirstDuplicate [ "+1", "-1" ])

example5 =
  testCase "Example Case 5" $ assertEqual [] 10 (findFirstDuplicate [ "+3", "+3", "+4", "-2", "-4" ])

example6 =
  testCase "Example Case 6" $ assertEqual [] 5 (findFirstDuplicate [ "-6", "+3", "+8", "+5", "-6" ])

example7 =
  testCase "Example Case 7" $ assertEqual [] 14 (findFirstDuplicate [ "+7", "+7", "-2", "-7", "-4" ])
