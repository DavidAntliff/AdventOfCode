module Day02 where
import qualified Data.Map as Map

-- Part 1 - count the number of occurrences of each character in the input string
--    - search for at least one count of '2',
--    - search for at least one count of '3',
-- Count up the number of strings that have at least one count of '2',
-- and the number of strings that have at least one count of '3', and
-- multiply these two counts together to get a "checksum".

record :: Map.Map Char Integer -> Char -> Map.Map Char Integer
record map k
  | k `Map.member` map = Map.insert k (map Map.! k + 1) map
  | otherwise = Map.insert k 1 map


histogram' :: Map.Map Char Integer -> String -> Map.Map Char Integer
histogram' tallies "" = tallies
histogram' tallies (x:xs) = histogram' (record tallies x) xs

histogram :: String -> Map.Map Char Integer
histogram = histogram' Map.empty


invert :: Map.Map Char Integer -> Map.Map Integer Char
invert m = Map.fromList pairs
  where pairs = [(v, k) | (k, v) <- Map.toList m]


getTally :: Map.Map Integer Char -> Integer -> Integer
getTally tallies i
   | i `Map.member` tallies = 1  -- "at least one"
   | otherwise = 0


getCounts :: String -> (Integer, Integer)
getCounts x = (getTally counts 2, getTally counts 3)
   where tallies = histogram x
         counts = invert $ tallies


checkSum :: [String] -> Integer
checkSum x = (sum $ fst $ uzc) * (sum $ snd $ uzc)
  where counts = map getCounts x
        uzc = unzip counts
