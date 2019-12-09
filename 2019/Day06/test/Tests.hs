import Lib

import Data.Map (Map)
import qualified Data.Map as Map

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ countNodesTest1
    , countNodesTest2
    , treeMapTest1
    , treeMapTest1
    , treeFoldTest1
    , treeFoldTest2
    , foldSubtreeTest1
    , foldSubtreeTest2
    , foldSubtreeTest3
    , foldSubtreeTest4
    , foldSubtreeTest5
    , depthTreeTest1
    , depthTreeTest2
    , depthTreeTest3
    , readInputTest1
    , countOrbitsTest1
    ]

tree1 = Tree 0 [ Tree 1 [ Tree 10 [] ]
               , Tree 2 [ Tree 20 [ Tree 21 [ Tree 22 [] ] ], Tree 30 [] ]
               , Tree 3 [ Tree 40 [ Tree 41 [ Tree 42 [] ] ] ]
               ]

--         G - H       J - K - L
--        /           /
-- COM - B - C - D - E - F
--                \
--                 I
tree_ex1 = Tree "COM" [ Tree "B" [ Tree "G" [ Tree "H" [] ]
                                 , Tree "C" [ Tree "D" [ Tree "I" []
                                                       , Tree "E" [ Tree "J" [ Tree "K" [ Tree "L" [] ] ]
                                                                  , Tree "F" [] ] ] ] ] ]

--         2 - 3       5 - 6 - 7
--        /           /
--   0 - 1 - 2 - 3 - 4 - 5
--                \
--                 4
tree_depth1 = Tree 0 [ Tree 1 [ Tree 2 [ Tree 3 [] ]
                              , Tree 2 [ Tree 3 [ Tree 4 []
                                                , Tree 4 [ Tree 5 [ Tree 6 [ Tree 7 [] ] ]
                                                         , Tree 5 [] ] ] ] ] ]

tree_ex1_input = [ "COM)B"
                 , "B)C"
                 , "C)D"
                 , "D)E"
                 , "E)F"
                 , "B)G"
                 , "G)H"
                 , "D)I"
                 , "E)J"
                 , "J)K"
                 , "K)L" ]

countNodesTest1 =
  testCase "countNodes test 1" $ assertEqual []
  1
  (countNodes $ Tree 0 [])

countNodesTest2 =
  testCase "countNodes test 2" $ assertEqual []
  12
  (countNodes $ tree1)

treeMapTest1 =
  testCase "treeMap test 1" $ assertEqual []
  (Tree 42 [])
  (treeMap (+ 42) (Tree 0 []))

treeMapTest2 =
  testCase "treeMap test 2" $ assertEqual []
  (Tree 42 [ Tree 43 [ Tree 52 [] ]
               , Tree 44 [ Tree 62 [ Tree 63 [ Tree 64 [] ] ], Tree 72 [] ]
               , Tree 45 [ Tree 82 [ Tree 83 [ Tree 84 [] ] ] ]
               ])
  (treeMap (+ 42) tree1)

treeFoldTest1 =
  testCase "treeFold test 1" $ assertEqual []
  17
  (treeFold (+) 17 (Tree 0 []))

treeFoldTest2 =
  testCase "treeFold test 2" $ assertEqual []
  240
  (treeFold (+) 8 tree1)

foldSubtreeTest1 =
  testCase "foldSubtree test 1" $ assertEqual []
  91
  (foldSubtree (\acc (Tree n _) -> acc + n * 2) 7 (Tree 42 []))

foldSubtreeTest2 =
  testCase "foldSubtree test 2" $ assertEqual []
  (7 + 1 * 2 + 2 * 2)
  (foldSubtree (\acc (Tree n _) -> acc + n * 2) 7 (Tree 1 [ Tree 2 [] ]))

foldSubtreeTest3 =
  testCase "foldSubtree test 3" $ assertEqual []
  0
  (foldSubtree (\acc t -> acc + countNodes t - 1) 0 (Tree 0 []))

foldSubtreeTest4 =
  testCase "foldSubtree test 4" $ assertEqual []
  42
  (foldSubtree (\acc t -> acc + countNodes t - 1) 0 tree_ex1)

foldSubtreeTest5 =
  testCase "foldSubtree test 5" $ assertEqual []
  54
  (foldSubtree (\acc t -> acc + countNodes t) 0 tree_ex1)

depthTreeTest1 =
  testCase "depthTree test 1" $ assertEqual []
  (Tree 0 [])
  (depthTree 0 (Tree 0 []))

depthTreeTest2 =
  testCase "depthTree test 2" $ assertEqual []
  (Tree 0 [ Tree 1 [] ])
  (depthTree 0 (Tree "A" [ Tree "B" [] ]))

depthTreeTest3 =
  testCase "depthTree test 3" $ assertEqual []
  tree_depth1
  (depthTree 0 tree_ex1)

readInputTest1 =
  testCase "readInput test 1" $ assertEqual []
  (Map.fromList [("B",["G","C"]),("C",["D"]),("COM",["B"]),("D",["I","E"]),("E",["J","F"]),("G",["H"]),("J",["K"]),("K",["L"])])
  (toMap $ readInput tree_ex1_input)

countOrbitsTest1 =
  testCase "countOrbits test 1" $ assertEqual []
  (42)
  (countOrbits 0 (toMap $ readInput tree_ex1_input) "COM")
