module Day03 where
import Data.Ord as Ord
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Either as Either
import Text.ParserCombinators.Parsec
import Control.Monad (void)
import qualified Claim

-- Part 1
--
-- 1000 x 1000 cells
-- Coordinates start at 0, 0
-- Find cells that are overlapped by two or more claims
--
-- Algorithm:
--
--  - Create a list of "column" views - each element in the list is a set of
--    claim IDs that coincide with the column, at an unknown row.
--  - Create a list of "row" views - each element in the list is a set of
--    claim IDs that coincide with the row, at an unknown column.
--  - Iterate through the cartesian product of these two lists and determine
--    the intersection of each pair.
--  - Count the number of intersections that are of length 2 or greater
--
-- Claim format: #id @ x,y: wxh
--
--
-- functions:
--
--   parseClaim: convert a String claim into a 5-tuple (id, x, y, w, h)

-- No longer needed:
--   sortClaims: sort all claims by top-left coordinate (row then column)
--   testClaim: given a cell coordinate, test if it falls within a claim
--   testClaims: given a cell coordinate, test if it falls within any active claims




sortClaims :: [Claim.Claim] -> [Claim.Claim]
sortClaims = List.sort


--testParse :: [String] -> [String]
--testParse = map (++ "y")

claimParser :: Parser Claim.Claim
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
  return (Claim.Claim (read id) (read x) (read y) (read w) (read h))


parseClaim :: String -> Either ParseError Claim.Claim
parseClaim x = parse claimParser "" x

parseClaims :: [String] -> [Claim.Claim]
parseClaims = Either.rights . map parseClaim

addClaim :: Map.Map Integer [Integer] -> (Claim.Claim -> Integer) -> (Claim.Claim -> Integer) -> Claim.Claim -> Map.Map Integer [Integer]
addClaim init extract_start extract_extent claim =
  let start_coord = extract_start claim
      end_coord = extract_start claim + extract_extent claim - 1
      new_list = [(x, [Claim.id claim]) | x <- [start_coord .. end_coord]]
      new_map = Map.fromList new_list
  in Map.unionWith (++) init new_map


columnView :: [Claim.Claim] -> Map.Map Integer [Integer]
columnView claims =
--  let c x y = addClaim y (\p -> Claim.x p) (\p -> Claim.w p) x
--  in foldr c Map.empty claims
  let c x y = addClaim x (\p -> Claim.x p) (\p -> Claim.w p) y
  in foldl c Map.empty claims

rowView :: [Claim.Claim] -> Map.Map Integer [Integer]
rowView claims =
  let c x y = addClaim x (\p -> Claim.y p) (\p -> Claim.h p) y
  in foldl c Map.empty claims


cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]


-- build a cell structure from the cartesian product
buildCell :: ((Integer, [Integer]), (Integer, [Integer])) -> ((Integer, Integer), [Integer])
buildCell prod =
  let ((col, ids1), (row, ids2)) = prod
  in ((col, row), List.intersect ids1 ids2)


combineViews :: Map.Map Integer [Integer] -> Map.Map Integer [Integer] -> [((Integer, Integer), [Integer])]
combineViews column_view row_view =
  let cart_prod = cartesianProduct (Map.toList column_view) (Map.toList row_view)
  in [ buildCell x | x <- cart_prod ]


findOverlappingClaims :: [((Integer, Integer), [Integer])] -> [Integer]
findOverlappingClaims x = List.foldr (++) [] (List.filter (\x -> length x >= 2) $ List.map (\((x,y),li) -> li) x)

countOverlaps :: [String] -> (Integer, Set.Set Integer)
countOverlaps lines =
  let claims = parseClaims lines
      row_view = rowView claims
      col_view = columnView claims
      combined_view = combineViews col_view row_view
      non_overlapping_claims = Set.difference (Set.fromList $ map (\x -> Claim.id x) claims) (Set.fromList $ findOverlappingClaims combined_view)
  in (toInteger $ length $ filter (>=2) $ map (\((_, _), ids) -> toInteger $ length ids) combined_view, non_overlapping_claims)

