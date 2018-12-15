-- NOT WORKING - COME BACK TO THIS ANOTHER TIME

module Day06 where
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map

-- • Read in coordinates from file, take note of max/min x and y values
-- • Identify coordinates by unique integer ID (position in file)
-- • Add coordinates to a "Live" list and also add them to a Map keyed by coordinate where value is the "owner's" ID (i.e. closest coordinate), and a live/dead flag.
-- • then for each coordinate in the live list, "grow" it into a new Map up/down/left/right with live flag set, adding these new coordinates to a new live list, and set itself with a "dead" flag in the map, removing itself from the live list (i.e. not adding itself to the new live list at all).
-- • If any grown-into map coordinate has a "dead" occupier, leave it there and don't grow into that location.
-- • If any map coordinate has a "live" occupier, mark that coordinate as "equidistant" (owner is 0 or something) and remove it from the live list (which is really a map now I think about it)
-- • If any new coordinate is a result of growing outside the x/y bounds, drop it
-- • Continue until the live list is empty
-- • Then go through and count the area per occupier.

type AreaID = Integer

data Coordinate = Coordinate { x :: Integer, y :: Integer } deriving (Ord, Eq, Show)

type AliveSet = Map.Map Coordinate AreaID

type Grid = Map.Map Coordinate AreaID

equidistant = 0 :: AreaID


parseCoordinate :: [String] -> Maybe Coordinate
parseCoordinate (x:y:_) = Just $ Coordinate (read x :: Integer) (read y :: Integer)
parseCoordinate _ = Nothing


growOneCell :: (Grid, AliveSet) -> Coordinate -> AreaID -> (Grid, AliveSet)
growOneCell (grid, alive) coord area_id =


  (Map.empty, Map.empty)


-- iterate through each alive member, resulting in a new grid state and new alive set
growOneGeneration :: (Grid, AliveSet) -> (Grid, AliveSet)
growOneGeneration (grid, alive) = Map.foldlWithKey growOneCell (grid, alive) alive


-- iterate through each generation
iterateGrid :: (Grid, AliveSet) -> (Grid, AliveSet)
iterateGrid (grid, alive)
  | Map.null alive = (grid, alive)
  | otherwise = iterateGrid (growOneGeneration (grid, alive))


-- grow :: Grid -> AliveSet -> Grid
-- grow grid alive
--   | Map.null alive = (grid, alive)
--   | otherwise =
--       let
--         (grid', alive') = iterateGrid grid alive
--       in grow grid' alive'


valuesFrequency :: (Ord k1, Num a) => Map.Map k2 k1 -> Map.Map k1 a
valuesFrequency = Map.foldl' (\mp x -> Map.insertWith (+) x 1 mp) Map.empty


-- countAreas :: Grid -> [Integer]
-- countAreas grid = valuesFrequency grid


findLargestArea :: Grid -> AliveSet -> Integer
findLargestArea grid alive =
  let
    final_grid = fst $ until (\(g, a) -> Map.null a) iterateGrid (grid, alive)
  in maximum $ valuesFrequency final_grid

parseInput :: [String] -> (Grid, AliveSet)
parseInput xs =
  let
    coordinates = map fromJust . map parseCoordinate . map (splitOn ", ") $ xs
    grid = Map.fromList (zip coordinates [1..])
    alive = grid  -- initially, they are the same map
  in (grid, alive)

part1 :: [String] -> Integer
--part1 = findLargestArea . map fromJust . map parseCoordinate . map (splitOn ", ")
part1 = uncurry findLargestArea . parseInput




-- s <- readFile "input.txt"
-- (grid, alive) = parseInput $ lines s
