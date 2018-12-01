module Day01 where

-- remove leading '+' characters from input strings
stripPlus :: String -> String
stripPlus ('+':x) = x
stripPlus x = x

calibrate :: (Num a, Read a) => [String] -> a
calibrate = sum . map read . map stripPlus

