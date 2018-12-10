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
  let
    result = startReact x
  in (result, toInteger $ length result)


until'' :: (a -> a -> Bool) -> (a -> a) -> a -> a
until'' p f x =
  let result = f x
  in case p x result of True -> until'' p f result
                        False -> x


until' :: (a -> a -> Bool) -> (a -> a) -> a -> a
until' p f = go
  where
    go x | let h = f(x) in p x h = x
         | otherwise             = go (f x)

-- until' :: (a -> b -> Bool) -> (a -> a) -> a -> a
-- until' p f = go
--   where
--     go x | p x 


-- react :: ([Char], Integer) -> ([Char], Integer)
-- react ([], n) = ([], n)
-- react ([x], n) = ([x], n)
-- react ((x:y:xs), n) =
--   if ord x `xor` ord y == 32 then
--     react (xs, n + 1)
--   else
--     let z = react (xs, n)
--     in (x:y:(fst z), snd z)


-- reactPolymer :: [Char] -> [Char]
-- reactPolymer x = fst $ until (\(y, n) -> n == 0) react (x, 0)

