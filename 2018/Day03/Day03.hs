module Day03 where

-- Part 1
--
-- 1000 x 1000 cells - make it 1001w x 1000h to ensure bottom-right coordinates are never on the edge
-- Find cells that are overlapped by two or more claims
-- Sort by top-left corner, then scan left/right -> top/bottom
-- Keep a list of "active" cells (haven't passed bottom-right corner yet)
--   - if current cell matches top-left coordinate of a claim, add it to active list
--   - if current cell matches bottom-right coordinate of a claim, remove from active list after the test
-- Only test active claims for overlap
-- Coordinates start at 0, 0

-- claim format: #id @ x,y: wxh



