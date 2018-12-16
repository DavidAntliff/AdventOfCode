module Day07 where
import Data.Graph.Inductive.Graph (Graph, mkGraph, indeg, nodes, suc, out)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (sort)


part1 :: [String] -> String
part1 xs = "TODO"


newtype NodeLabel = NodeLabel Char
type EdgeLabel = ()


-- Kahn Algorithm - provide graph and "S" - list of starting nodes.
-- Return the ordered list of traversal.
-- Note that ties are broken by sorting on node labels.
kahnAlgorithm :: (Graph gr, Num b1, Enum b1) => gr a b2 -> [b1] -> [b1] -> [b1]
kahnAlgorithm _ [] l = l
kahnAlgorithm graph s l =
  let s' = sort s
      n = head s'
      l' = l ++ [n]
      s'' = (tail s') ++ suc graph n
      graph' = foldl (\acc x -> delLEdge x acc) graph (out graph n)
  in kahnAlgorithm graph' s'' l'

findStartingNodes :: (Graph gr, Num b1, Enum b1) => gr a b2 -> [b1]
findStartingNodes graph = map fst . filter (\(x, y) -> y == 0) $ zip [1..] $ map (indeg graph) (nodes graph)

topologicalSort :: (Graph gr, Num b1, Enum b1) => gr a b2 -> [b1]
topologicalSort graph = kahnAlgorithm graph [] $ findStartingNodes graph

genTestGraph :: Gr Char ()
genTestGraph = mkGraph (zip [1..] "ABCDEF") [(3,1,()),(3,6,()),(1,2,()),(1,4,()),(2,5,()),(4,5,()),(6,5,())]

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




