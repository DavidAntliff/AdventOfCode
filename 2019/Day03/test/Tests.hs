{-# LANGUAGE DuplicateRecordFields #-}

import Lib
import Linear.V2
import qualified Data.Set as Set
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ parseMovementRightTest1
    , parseMovementRightTest2
    , parseMovementLeftTest1
    , parseMovementBadTest1
    , parseMovementUpTest1
    , parseMovementDownTest1
    , parseWirePathTest1
    , parseWirePathTest2
    , parseWirePathTest3
    , manhattanDistanceTest1
    , manhattanDistanceTest2
    , manhattanDistanceTest3
    , walkLineTest1
    , walkLineTest2
    , walkLineTest3
    , walkLineTest4
    , walkPathTest1
    , walkPathTest2
    , walkPathTest3
    , walkPathTest4
    , constructWireVisitsTest1
    , constructWireVisitsTest2
    , constructWireVisitsTest3
    ]


parseMovementRightTest1 =
  testCase "parseMovement Right test 1" $ assertEqual []
  (V2 7 0)
  (parseMovement "R7")

parseMovementRightTest2 =
  testCase "parseMovement Right test 2" $ assertEqual []
  (V2 421990 0)
  (parseMovement "R421990")

parseMovementLeftTest1 =
  testCase "parseMovement Left test 1" $ assertEqual []
  (V2 (-42) 0)
  (parseMovement "L42")

parseMovementUpTest1 =
  testCase "parseMovement Up test 1" $ assertEqual []
  (V2 0 12456)
  (parseMovement "U12456")

parseMovementDownTest1 =
  testCase "parseMovement Down test 1" $ assertEqual []
  (V2 0 (-9998))
  (parseMovement "D9998")

parseMovementBadTest1 =
  testCase "parseMovement Bad test 1" $ assertEqual []
  (V2 0 0)
  (parseMovement "789")

parseWirePathTest1 =
  testCase "parseWirePath test 1 - empty string" $ assertEqual []
  [ V2 0 0 ]
  (parseWirePath "")

parseWirePathTest2 =
  testCase "parseWirePath test 2" $ assertEqual []
  [ V2 4 0 ]
  (parseWirePath "R4")

parseWirePathTest3 =
  testCase "parseWirePath test 3" $ assertEqual []
  [ V2 (-7) 0
  , V2 0 99
  , V2 42 0
  , V2 0 (-771)
  ]
  (parseWirePath "L7,U99,R42,D771")

manhattanDistanceTest1 =
  testCase "manhattanDistance test 1" $ assertEqual []
  0
  (manhattanDistance $ V2 0 0)

manhattanDistanceTest2 =
  testCase "manhattanDistance test 2" $ assertEqual []
  13
  (manhattanDistance $ V2 13 0)

manhattanDistanceTest3 =
  testCase "manhattanDistance test 3" $ assertEqual []
  74
  (manhattanDistance $ V2 0 (-74))

-- should not include the starting point
walkLineTest1 =
  testCase "walkLine test 1" $ assertEqual []
  []
  (walkLine (V2 0 0) (V2 0 0))

walkLineTest2 =
  testCase "walkLine test 2" $ assertEqual []
  []
  (walkLine (V2 7 (-88)) (V2 0 0))

walkLineTest3 =
  testCase "walkLine test 3" $ assertEqual []
  [ V2 1 0
  , V2 2 0
  ]
  (walkLine (V2 0 0) (V2 2 0))

walkLineTest4 =
  testCase "walkLine test 4" $ assertEqual []
  [ V2 (-1) 0
  , V2 (-2) 0
  , V2 (-3) 0
  ]
  (walkLine (V2 0 0) (V2 (-3) 0))



walkPathTest1 =
  testCase "walkPath test 1" $ assertEqual []
  []
  (walkPath (V2 0 0) [])

walkPathTest2 =
  testCase "walkPath test 2" $ assertEqual []
  []
  (walkPath (V2 0 0) [V2 0 0])

walkPathTest3 =
  testCase "walkPath test 3" $ assertEqual []
  [ V2 1 0
  , V2 2 0
  , V2 3 0
  ]
  (walkPath (V2 0 0) [ V2 3 0 ])

walkPathTest4 =
  testCase "walkPath test 4" $ assertEqual []
  [ V2 1 0
  , V2 2 0
  , V2 3 0
  , V2 3 1
  , V2 3 2
  , V2 3 3
  , V2 3 4
  , V2 2 4
  , V2 1 4
  ]
  (walkPath (V2 0 0) [ V2 3 0
                     , V2 0 4
                     , V2 (-2) 0
                     ])

constructWireVisitsTest1 =  -- no move
  testCase "constructWireVisits test 1" $ assertEqual []
  Set.empty
  (constructWireVisits [])

constructWireVisitsTest2 =  -- move, but nowhere
  testCase "constructWireVisits test 2" $ assertEqual []
  Set.empty
  (constructWireVisits [ V2 0 0 ])

constructWireVisitsTest3 =
  testCase "constructWireVisits test 3" $ assertEqual []
  (Set.fromList [ V2 1 0
                , V2 2 0
                , V2 3 0
                , V2 3 (-1)
                , V2 3 (-2)
                ])
  (constructWireVisits [ V2 3 0
                       , V2 0 (-2)
                       ])


