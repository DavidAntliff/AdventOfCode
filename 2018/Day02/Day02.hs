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


-- Part 2

-- Find hamming distance between each pair of strings, looking for the one
-- with a distance of exactly 1.
--
-- Naive approach: for each head of string list, compare with every item in tail,
-- terminating comparison as soon as two characters differ.


-- findPair - given a list of strings, find the two with hamming distance exactly 1
findPair :: [String] -> Maybe (String, String)
findPair [] = Nothing
findPair (x:xs)
  | result /= Nothing = Just (x, case result of Just a -> a)
  | otherwise = findPair xs
  where result = compareHead (x:xs)


findCommon :: Maybe (String, String) -> Maybe String
findCommon x = case x of Nothing -> Nothing
                         Just (i, j) -> Just $ map fst $ filter (\(a, b) -> a == b) $ zip i j


-- compareHead - given a list of strings, calculate the hamming distance between the head and every item in the tail.
-- if a hamming distance of 1 is found, return Just match, otherwise return Nothing.
compareHead :: [String] -> Maybe String
compareHead [] = Nothing
compareHead [x] = Nothing
compareHead (x:xs)
  | result /= [] = Just (head result)
  | otherwise = Nothing
  where result = take 1 $ filter (\a -> hammingDistance x a == Just 1) xs


hammingDistance :: String -> String -> Maybe Integer
hammingDistance x y
  | (length x) /= (length y) = Nothing
  | otherwise = Just $ toInteger $ sum [ fromEnum (el1 /= el2) | (el1, el2) <- zip x y ]
-- Nice alternative:
--  | otherwise = (Just .) . (toInteger .) . (sum .) .  zipWith ((fromEnum .) . (/=))



