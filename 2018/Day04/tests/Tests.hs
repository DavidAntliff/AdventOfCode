import Day04
import Record
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ testParseRecord1, testParseRecord2, testParseRecord3
    ]

-- Part 1

-- ex1 = testCase "Part 1 Example Case 1" $ assertEqual [] (0, 0) (getCounts "abcdef")

testParseRecord1 = testCase "Test parse record" $ assertEqual []
                   (Record { datetime = DateTime {year = 1518, month = 8, day = 17, hour = 0, minute = 1}, event = Begin 3529})
                   (parseRecord "[1518-08-17 00:01] Guard #3529 begins shift")

testParseRecord2 = testCase "Test parse record" $ assertEqual []
                   (Record { datetime = DateTime {year = 1518, month = 8, day = 17, hour = 0, minute = 1}, event = Sleep})
                   (parseRecord "[1518-08-17 00:01] falls asleep")

testParseRecord3 = testCase "Test parse record" $ assertEqual []
                   (Record { datetime = DateTime {year = 1518, month = 8, day = 17, hour = 0, minute = 1}, event = Wake})
                   (parseRecord "[1518-08-17 00:01] wakes up")


