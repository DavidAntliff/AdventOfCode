import Day04
import Record
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ testParseRecord1, testParseRecord2, testParseRecord3
    , testSplitRecords1
    , testCalcShift1
    , testCalcSleepTime1
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


testSplitRecords1 = testCase "Test split records" $ assertEqual []
                    [ [Record (DateTime 1 2 3 4 5) (Begin 100),
                       Record (DateTime 1 2 3 4 8) Sleep,
                       Record (DateTime 1 2 3 4 11) Wake],
                      [Record (DateTime 1 2 4 4 6) (Begin 200),
                       Record (DateTime 1 2 4 4 7) Sleep,
                       Record (DateTime 1 2 4 4 12) Wake]]
                    (splitRecords [Record (DateTime 1 2 3 4 5) (Begin 100),
                                   Record (DateTime 1 2 3 4 8) Sleep,
                                   Record (DateTime 1 2 3 4 11) Wake,
                                   Record (DateTime 1 2 4 4 6) (Begin 200),
                                   Record (DateTime 1 2 4 4 7) Sleep,
                                   Record (DateTime 1 2 4 4 12) Wake])


testCalcShift1 = testCase "Test calc shift" $ assertEqual []
                 (100, 3)
                 (calcShift [ Record (DateTime 1 2 3 4 5) (Begin 100),
                              Record (DateTime 1 2 3 4 8) Sleep,
                              Record (DateTime 1 2 3 4 11) Wake ])


testCalcSleepTime1 = testCase "Test calc sleep time" $ assertEqual []
                     10
                     (calcSleepTime [Record {datetime = DateTime {year = 1518, month = 2, day = 11, hour = 0, minute = 0}, event = Begin 347},
                                     Record {datetime = DateTime {year = 1518, month = 2, day = 11, hour = 0, minute = 35}, event = Sleep},
                                     Record {datetime = DateTime {year = 1518, month = 2, day = 11, hour = 0, minute = 39}, event = Wake},
                                     Record {datetime = DateTime {year = 1518, month = 2, day = 11, hour = 0, minute = 53}, event = Sleep},
                                     Record {datetime = DateTime {year = 1518, month = 2, day = 11, hour = 0, minute = 59}, event = Wake}])
