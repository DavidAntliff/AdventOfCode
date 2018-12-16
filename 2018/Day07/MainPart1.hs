module Main where
import Day07
import System.IO (getContents)
import Data.Char (ord, chr)

main :: IO()
main = do
  contents <- getContents
  let l = lines contents
      edges = buildEdges l
      nodes = buildNodes edges
      graph = buildGraph nodes edges
      result = topologicalSort graph
  print nodes
  print edges
  print $ map chr $ map (+ ord 'A') result
