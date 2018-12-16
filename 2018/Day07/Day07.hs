module Day07 where
import Data.Graph.Inductive.Graph (Graph, mkGraph, indeg, nodes, suc, out, pre, delLEdge)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Char (ord)


type NodeLabel = Char
type EdgeLabel = ()


-- Kahn Algorithm - provide graph and "S" - list of starting nodes.
-- Return the ordered list of traversal.
-- Note that ties are broken by sorting on node labels.
-- https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
--kahnAlgorithm :: (Graph gr, Num b1, Enum b1) => gr a b2 -> [b1] -> [b1] -> [b1]
kahnAlgorithm :: Gr NodeLabel EdgeLabel -> [Int] -> [Int] -> [Int]
kahnAlgorithm _ [] l = l
kahnAlgorithm graph s l =
  let s' = sort s  -- alphabetical ordering
      n = head s'
      l' = l ++ [n]
      sucs = suc graph n
      -- remove edges out from this node:
      graph' = foldl (\acc x -> delLEdge x acc) graph (out graph n)
      -- only append to s if each successor has no other incoming edges:
      t = map fst $ filter (\x -> snd x == []) $ zip (sucs) (map (pre graph') $ sucs)
      s'' = (tail s') ++ t
  in kahnAlgorithm graph' s'' l'

--findStartingNodes :: (Graph gr, Num b1, Enum b1) => gr a b2 -> [b1]
findStartingNodes :: Gr NodeLabel EdgeLabel -> [Int]
findStartingNodes graph = map fst . filter (\(x, y) -> y == 0) $ zip [0..] $ map (indeg graph) (nodes graph)

--topologicalSort :: (Graph gr, Num b1, Enum b1) => gr a b2 -> [b1]
topologicalSort :: Gr NodeLabel EdgeLabel -> [Int]
topologicalSort graph = kahnAlgorithm graph (findStartingNodes graph) []

buildEdges :: [String] -> [(Char, Char)]
buildEdges xs = map (\x -> (x !! 1 !! 0, x !! 7 !! 0)) $ map words $ xs

buildNodes :: [(Char, Char)] -> [Char]
buildNodes edges =
  let node_list = foldl (++) "" $ (\(x, y) -> [x] ++ [y]) <$> edges
  in Set.toList $ Set.fromList node_list

buildGraph :: [Char] -> [(Char, Char)] -> Gr NodeLabel EdgeLabel
buildGraph nodes edges =
  let edges' = map (\(x, y) -> (ord x - ord 'A', ord y - ord 'A', ())) edges
  in mkGraph (zip [0..] nodes) edges'

genTestGraph :: Gr NodeLabel EdgeLabel
genTestGraph = mkGraph (zip [0..] "ABCDEF") [(2,0,()),(2,5,()),(0,1,()),(0,3,()),(1,4,()),(3,4,()),(5,4,())]

-- Given a graph:
--    λ> g1 = genTestGraph
--    λ> :t g1
--    g1 :: Gr Char ()
--
--  Find all nodes with only outgoing edges:
--    λ> import Data.Graph.Inductive.Graph (nodes, indeg)
--    λ> map fst . filter (\(x, y) -> y == 0) $ zip [1..] $ map (indeg g1) (nodes g1)
--    [3]
--
--  Obtain destination of all outgoing edges from a node:
--    λ> suc g1 3
--    [1,6]
--
--  Remove outgoing edges from a node:
--    λ> g2 = foldl (\acc x -> delLEdge x acc) g1 (out g1 3)
--
--  Update s:
--    n = head $ sort s
--    s' = (tail s) ++ suc g2 n
--    l' = l ++ [n]




