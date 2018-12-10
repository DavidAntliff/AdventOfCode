module Day05 where
import Data.Bits
import Data.Char

react :: [Char] -> [Char]
react [] = []
react [x] = [x]
react (x:y:xs) = if ord x `xor` ord y == 32 then react xs else x:(react (y:xs))

startReact :: [Char] -> [Char]
startReact x = until'' (\m n -> length m == length n) react x

reactPolymer :: [Char] -> ([Char], Integer)
reactPolymer x =
  let result = startReact x
  in (result, toInteger $ length result)


until'' :: (a -> a -> Bool) -> (a -> a) -> a -> a
until'' p f x =
  let result = f x
  in case p x result of False -> until'' p f result
                        True -> result

