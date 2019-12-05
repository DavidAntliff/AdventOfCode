module Main where

import Lib

main = interact $ show . part1 . lines


-- Notes:
--
-- Coordinates are (x, y) where x is horizontal displacement from origin,
-- and y is vertical displacement from origin.
--
-- Each wire starts at (0, 0)
--
-- Each input line describes a wire, so U increases y, D decreases y, L decreases x, R increases x.
-- Each wire therefore visits a set of coordinates.
--
-- Two wires cross if they visit the same coordinates.
--
-- Find the set of coordinate pairs each wire visits, then
-- find any coordinate pairs that exist for both wires (consider sorting by Manhattan distance first)
--
-- For each matching coordinate pair, calculate Manhattan distance, sort by distance
--
-- Select the smallest distance and display it.
--
