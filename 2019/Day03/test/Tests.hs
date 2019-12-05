{-# LANGUAGE DuplicateRecordFields #-}

import Lib
import Coordinate
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
  (Coordinate 7 0)
  (parseMovement "R7")

parseMovementRightTest2 =
  testCase "parseMovement Right test 2" $ assertEqual []
  (Coordinate (421990) 0)
  (parseMovement "R421990")

parseMovementLeftTest1 =
  testCase "parseMovement Left test 1" $ assertEqual []
  (Coordinate (-42) 0)
  (parseMovement "L42")

parseMovementUpTest1 =
  testCase "parseMovement Up test 1" $ assertEqual []
  (Coordinate 0 12456)
  (parseMovement "U12456")

parseMovementDownTest1 =
  testCase "parseMovement Down test 1" $ assertEqual []
  (Coordinate 0 (-9998))
  (parseMovement "D9998")

parseMovementBadTest1 =
  testCase "parseMovement Bad test 1" $ assertEqual []
  (Coordinate 0 0)
  (parseMovement "789")

parseWirePathTest1 =
  testCase "parseWirePath test 1 - empty string" $ assertEqual []
  WirePath { movements = [ Coordinate 0 0 ] }
  (parseWirePath "")

parseWirePathTest2 =
  testCase "parseWirePath test 2" $ assertEqual []
  WirePath { movements = [ Coordinate 4 0 ] }
  (parseWirePath "R4")

parseWirePathTest3 =
  testCase "parseWirePath test 3" $ assertEqual []
  WirePath { movements = [ Coordinate (-7) 0,
                           Coordinate 0 99,
                           Coordinate 42 0,
                           Coordinate 0 (-771) ] }
  (parseWirePath "L7,U99,R42,D771")

manhattanDistanceTest1 =
  testCase "manhattanDistance test 1" $ assertEqual []
  0
  (manhattanDistance $ Coordinate 0 0)

manhattanDistanceTest2 =
  testCase "manhattanDistance test 2" $ assertEqual []
  13
  (manhattanDistance $ Coordinate 13 0)

manhattanDistanceTest3 =
  testCase "manhattanDistance test 3" $ assertEqual []
  74
  (manhattanDistance $ Coordinate 0 (-74))

-- should not include the starting point
walkLineTest1 =
  testCase "walkLine test 1" $ assertEqual []
  []
  (walkLine (Coordinate 0 0) (Coordinate 0 0))

walkLineTest2 =
  testCase "walkLine test 2" $ assertEqual []
  []
  (walkLine (Coordinate 7 (-88)) (Coordinate 0 0))

walkLineTest3 =
  testCase "walkLine test 3" $ assertEqual []
  [ Coordinate 1 0
  , Coordinate 2 0
  ]
  (walkLine (Coordinate 0 0) (Coordinate 2 0))

walkLineTest4 =
  testCase "walkLine test 4" $ assertEqual []
  [ Coordinate (-1) 0
  , Coordinate (-2) 0
  , Coordinate (-3) 0
  ]
  (walkLine (Coordinate 0 0) (Coordinate (-3) 0))



walkPathTest1 =
  testCase "walkPath test 1" $ assertEqual []
  []
  (walkPath (Coordinate 0 0) [])

walkPathTest2 =
  testCase "walkPath test 2" $ assertEqual []
  []
  (walkPath (Coordinate 0 0) [Coordinate 0 0])

walkPathTest3 =
  testCase "walkPath test 3" $ assertEqual []
  [ Coordinate 1 0
  , Coordinate 2 0
  , Coordinate 3 0
  ]
  (walkPath (Coordinate 0 0) [ Coordinate 3 0 ])

walkPathTest4 =
  testCase "walkPath test 4" $ assertEqual []
  [ Coordinate 1 0
  , Coordinate 2 0
  , Coordinate 3 0
  , Coordinate 3 1
  , Coordinate 3 2
  , Coordinate 3 3
  , Coordinate 3 4
  , Coordinate 2 4
  , Coordinate 1 4
  ]
  (walkPath (Coordinate 0 0) [ Coordinate 3 0
                             , Coordinate 0 4
                             , Coordinate (-2) 0
                             ])

constructWireVisitsTest1 =  -- no move
  testCase "constructWireVisits test 1" $ assertEqual []
  WireVisits { coordinates = [] }
  (constructWireVisits WirePath { movements = [] })

constructWireVisitsTest2 =  -- move, but nowhere
  testCase "constructWireVisits test 2" $ assertEqual []
  WireVisits { coordinates = [] }
  (constructWireVisits WirePath { movements = [ Coordinate 0 0 ] })

constructWireVisitsTest3 =
  testCase "constructWireVisits test 3" $ assertEqual []
  WireVisits { coordinates = [ Coordinate 1 0
                             , Coordinate 2 0
                             , Coordinate 3 0
                             , Coordinate 3 (-1)
                             , Coordinate 3 (-2)
                             ] }
  (constructWireVisits WirePath { movements = [ Coordinate 3 0
                                              , Coordinate 0 (-2)
                                              ] })


