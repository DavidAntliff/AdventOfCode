module Lib where

import Data.List.Split (splitOn)

import Data.Map (Map)
import qualified Data.Map as Map

data Tree a = Tree a [Tree a] deriving (Eq, Show)

countNodes :: Tree a -> Int
countNodes (Tree _ []) = 1
countNodes (Tree _ cs) = (+) 1 $ sum $ map countNodes cs

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap p (Tree n []) = Tree (p n) []
treeMap p (Tree n cs) = Tree (p n) (map (treeMap p) cs)

-- fold over node values
-- fold over children first, then node value
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold p acc (Tree n []) = p acc n
treeFold p acc (Tree n cs) = p (foldl (treeFold p) acc cs) n

-- fold over all subtrees, children first
foldSubtree :: (b -> Tree a -> b) -> b -> Tree a -> b
foldSubtree p acc t@(Tree n []) = p acc t
foldSubtree p acc t@(Tree n cs) = p (foldl (foldSubtree p) acc cs) t

-- visit every node, creating a new tree where the value is the depth of the original node
depthTree :: (Num b) => b -> Tree a -> Tree b
depthTree depth (Tree n []) = Tree depth []
depthTree depth (Tree n cs) =
  let cs' = map (depthTree (depth + 1)) cs
  in Tree depth cs'

-- load orbits from list of "AAA)BBB" rules
-- buildTree :: [String] -> Tree String
-- buildTree ss =
--   let map splitOn (")")


readInput :: [String] -> [(String, [String])]
readInput = map (\[x, y] -> (x, [y])) . map (splitOn ")")

toMap :: [(String, [String])] -> Map String [String]
toMap = Map.fromListWith (++)

-- given a map and a key, count the size of all the subtrees
countOrbits :: Int -> Map String [String] -> String -> Int
countOrbits acc m key = case Map.lookup key m of
                            Just value -> (+) acc $ sum $ map (countOrbits (acc + 1) m) value
                            Nothing -> acc

-- an object can be orbited by different things
-- but a thing can only directly orbit one thing
part1 :: [String] -> Int
part1 ss =
  let m = toMap $ readInput ss
      num_orbits = countOrbits 0 m "COM"
  in num_orbits



