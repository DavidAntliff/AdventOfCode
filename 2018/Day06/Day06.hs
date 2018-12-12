module Day06 where
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

data Coordinate = Coordinate { x :: Integer, y :: Integer } deriving Show

type AliveSet = Map.Map Coordinate AreaID

type Grid = Map.Map Coordinate AreaID

equidistant = 0 :: AreaID


parseCoordinate :: [String] -> Maybe Coordinate
parseCoordinate (x:y:_) = Just $ Coordinate (read x :: Integer) (read y :: Integer)
parseCoordinate _ = Nothing


iterateGrid :: Grid -> AliveSet -> (Grid, AliveSet)
iterateGrid grid alive = (grid, alive)


grow :: Grid -> AliveSet -> (Grid, AliveSet)
grow grid alive
  | Map.null alive = (grid, alive)
  | otherwise =
      let
        (grid', alive') = iterateGrid grid alive
      in grow grid' alive'


findLargestArea :: [Coordinate] -> Integer
findLargestArea x = 0

part1 :: [String] -> Integer
part1 = findLargestArea . map fromJust . map parseCoordinate . map (splitOn ", ")

