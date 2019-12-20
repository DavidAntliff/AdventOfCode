import Lib

import qualified Data.Set as Set

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ readAsteroidMapLineTest1
    , readAsteroidMapLineTest2
    , readAsteroidMapLineTest3
    , readAsteroidMapTest1
    , readAsteroidMapTest2
    ]


testMap1 = "\
\....\n\
\....\n\
\....\n\
\...."

testMap2 = "\
\#..\n\
\.#.\n\
\.##"

readAsteroidMapLineTest1 =
  testCase "readAsteroidMapLine test 1" $ assertEqual []
  (Set.fromList [])
  (readAsteroidMapLine (42, ".........."))

readAsteroidMapLineTest2 =
  testCase "readAsteroidMapLine test 2" $ assertEqual []
  (Set.fromList [(0,42), (3,42), (5,42)])
  (readAsteroidMapLine (42, "#..#.#...."))

readAsteroidMapLineTest3 =
  testCase "readAsteroidMapLine test 3" $ assertEqual []
  (Set.fromList [(1,2), (2,2)])
  (readAsteroidMapLine (2, ".##"))

readAsteroidMapTest1 =
  testCase "readAsteroidMap test 1" $ assertEqual []
  (Set.fromList [])
  (readAsteroidMap testMap1)

readAsteroidMapTest2 =
  testCase "readAsteroidMap test 2" $ assertEqual []
  (Set.fromList [(0,0), (1,1), (1,2), (2,2)])
  (readAsteroidMap testMap2)

