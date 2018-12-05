module Day03 where
import Data.Ord as Ord
import qualified Data.List
import Text.ParserCombinators.Parsec
import Control.Monad (void)

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


-- functions:
--
--   parseClaim: convert a String claim into a 5-tuple (id, x, y, w, h)
--   sortClaims: sort all claims by top-left coordinate (row then column)
--   testClaim: given a cell coordinate, test if it falls within a claim
--   testClaims: given a cell coordinate, test if it falls within any active claims
--   addClaims: given a cell coordinate, the sorted list of claims, and the current
--              active list, add any claims that start at the coordinate to the active list (may be multiple)
--   removeClaims: given a cell coordinate and the current active list, remove any
--                 claims that end on that coordinate


data Claim = Claim { id :: Integer
                   , x :: Integer
                   , y :: Integer
                   , w :: Integer
                   , h :: Integer
                   } deriving (Show)

instance Eq Claim where
  (Claim _ x1 y1 w1 h1) == (Claim _ x2 y2 w2 h2) = x1 == x2 && y1 == y2 && w1 == w2 && h1 == h2

instance Ord Claim where
  compare = comparing y <> comparing x



sortClaims :: [Claim] -> [Claim]
sortClaims = Data.List.sort



--testParse :: [String] -> [String]
--testParse = map (++ "y")

claimParser :: Parser Claim
claimParser = do
  void $ char '#'
  id <- many1 digit
  void $ string " @ "
  x <- many1 digit
  void $ char ','
  y <- many1 digit
  void $ string ": "
  w <- many1 digit
  void $ char 'x'
  h <- many1 digit
  return (Claim (read id) (read x) (read y) (read w) (read h))


parseClaim :: String -> Either ParseError Claim
parseClaim x = parse claimParser "" x

testParse :: [String] -> [Either ParseError Claim]
testParse = map parseClaim
