module Main where

import Lib (part1)

-- The number of orbits is equal to the sum of all subtree sizes,
-- minus the root node of each subtree. Therefore:
--
-- num_orbits = foldSubtree (\acc t -> acc + countNodes t - 1) 0 tree

main = interact $ show . part1 . lines
