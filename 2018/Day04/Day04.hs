module Day04 where
import Record
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Maybe


-- Part 1

parseRecords :: [String] -> [Record]
parseRecords = map parseRecord


sortRecords :: [Record] -> [Record]
sortRecords = List.sortOn (\x -> datetime x)


-- Basic algorithm (on sorted list of Records)
--   - split records list a list of shifts (each associated with one guard)
--   - for each shift, calculate sleep vector for 60 minutes, where 0 is awake, 1 is asleep
--     - add this to a cumulative vector of sleep time per guard
--   - analyse the resulting sleep vectors for:
--     - a). the highest sum and hence the sleepiest guard, and
--     - b). the minute that this guard spends the most time asleep
--   - the resulting value is the minute * sleepiest guard ID


-- Split all records into "per-guard" shift lists
-- Guarantees that the first element of each shift list is a Begin event
-- (Remove the head of the list since it will not have an identifying guard ID)
splitRecords :: [Record] -> [[Record]]
splitRecords = tail . Split.split (Split.keepDelimsL $ Split.whenElt (\x -> isBegin (event x)))


-- For a list of Records, extract the guard ID and calculate the accumulated
-- sleep time, returning the result in a tuple:
calcShift :: [Record] -> (Integer, Integer)
calcShift x =
  let guard_id = case event (head x) of Begin id -> id
                                        _ -> error "Head of list isn't a Begin event"
      sleep_time = calcSleepTime x
  in (guard_id, sleep_time)


-- Total sleep time is (sum of wake times) - (sum of sleep times)
calcSleepTime :: [Record] -> Integer
calcSleepTime x =
  let wake_sum = sum [ minute $ datetime x | x@(Record _ Wake) <- x]
      sleep_sum = sum [ minute $ datetime x | x@(Record _ Sleep) <- x]
  in wake_sum - sleep_sum


makeEntryVector :: Event -> Integer -> [Integer]
makeEntryVector (Begin _) _ = [ 0 | x <- [0..59] ]
makeEntryVector Wake  min = [ if x < min then 0 else (-1) | x <- [0..59] ]
makeEntryVector Sleep min = [ if x < min then 0 else 1 | x <- [0..59] ]


-- length 60 list showing 1 for sleeping minutes, 0 for awake minutes.
makeShiftVector :: [Record] -> [Integer]
makeShiftVector shift =
  let params = map (\x -> (event x, minute $ datetime x)) shift
      list = map (uncurry makeEntryVector) params
  in foldr (zipWith (+)) (repeat 0) list


addShift :: [Record] -> Integer -> Map.Map Integer [Integer] -> Map.Map Integer [Integer]
addShift shift guard_id acc = Map.insertWith (zipWith (+)) guard_id (makeShiftVector shift) acc


addShifts :: [[Record]] -> Map.Map Integer [Integer] -> Map.Map Integer [Integer]
addShifts [] acc = acc
addShifts (x:xs) acc =
  let guard_id = fromBegin $ event (x !! 0)
  in addShifts xs (addShift x guard_id acc)


keyForMaxValueFromMap :: Ord a => Map.Map k a -> [k]
keyForMaxValueFromMap xs = Map.keys $ Map.filter (== m) xs
  where m = maximum $ Map.elems xs


calcLongestSleeper :: [[Record]] -> (Integer, Integer)
calcLongestSleeper shifts =
  let shift_vector_by_guard = addShifts shifts Map.empty
      sleep_total_by_guard = Map.map (\x -> sum x) shift_vector_by_guard
      sleepiest_guard_id = keyForMaxValueFromMap sleep_total_by_guard !! 0
      sleepiest_guard_vector = fromJust $ Map.lookup sleepiest_guard_id shift_vector_by_guard
      max_sleep = maximum sleepiest_guard_vector
      max_sleep_minute = toInteger $ fromJust $ List.elemIndex max_sleep sleepiest_guard_vector
      -- part 2
      (frequent_guard_id, frequent_minute) = mostFrequentMinute shift_vector_by_guard
  in (sleepiest_guard_id * max_sleep_minute, frequent_guard_id * frequent_minute)


-- Part 2
-- Determine the guard who slept the longest for any minute and return the guard's ID and the minute
mostFrequentMinute :: Map.Map Integer [Integer] -> (Integer, Integer)
mostFrequentMinute shift_vector =
  let frequent_guard_id = keyForMaxValueFromMap shift_vector !! 0
      frequent_guard_vector = fromJust $ Map.lookup frequent_guard_id shift_vector
      frequent_minute_max = maximum $ frequent_guard_vector
      frequent_minute = toInteger $ fromJust $ List.elemIndex frequent_minute_max frequent_guard_vector
  in (frequent_guard_id, frequent_minute)



