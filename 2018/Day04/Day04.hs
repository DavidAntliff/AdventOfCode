module Day04 where
import Record
import qualified Data.List as List
import qualified Data.List.Split as Split


-- Part 1

parseRecords :: [String] -> [Record]
parseRecords = map parseRecord


sortRecords :: [Record] -> [Record]
sortRecords = List.sortOn (\x -> datetime x)


-- Basic algorithm (on sorted list of Records)
--   - split list into a list per guard
--     - for each list, calculate sleep time
--     - add this to a running record of sleep time per guard


isBegin :: Event -> Bool
isBegin Begin{} = True
isBegin _ = False

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






calcLongestSleeper :: [[Record]] -> Integer
calcLongestSleeper x =
  let guard_id = 0
      selected_minute = 0
  in guard_id * selected_minute

