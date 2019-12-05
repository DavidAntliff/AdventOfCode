module Main where

import Lib

main = interact $ show . part2 . lines

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
-- Find the ordered set of coordinate pairs each wire visits, then
-- find the first intersection of both wires,
-- then find the position of this intersection for both wires and add distance
