module Main where

import Data.List (elemIndex, partition, sortOn, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

main :: IO ()
main = do
  (left, right) <- unzip . map (toTuple . splitOn " | ") . lines <$> getContents
  print $ solve1 (concatMap words right)
  print $ solve2 left right

solve1 :: [String] -> Int
solve1 = length . filter isKnown

solve2 :: [String] -> [String] -> Int
solve2 all toGuess = sum guessed
  where
    guessed = zipWith (\a b -> solveLine (words a) (words b)) all toGuess

solveLine :: [String] -> [String] -> Int
solveLine base toGuess = read guessed
  where
    (known, unknown) = partition isKnown base
    guessed = concatMap (`getNumber` sortOn length known) toGuess

isKnown :: Foldable t => t a -> Bool
isKnown s = (length s == 2) || (length s == 7) || length s == 3 || length s == 4

getNumber :: String -> [String] -> String
getNumber s knowns
  | length s == 2 = "1"
  | length s == 3 = "7"
  | length s == 4 = "4"
  | length s == 7 = "8"
  | otherwise = show $ infere knowns s

infere :: [String] -> String -> Int
infere base s = unkownNumbers !! ix
  where
    diff = diffKU base s
    ix = fromJust $ elemIndex diff diffKnownUnknown

--  a
-- b c
--  d
-- e f
--  g
numStrings :: [String]
--                0       1      2        3      4        5         6        7       8          9
numStrings = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

knownStrings :: [String]
knownStrings = sortOn length $ map (numStrings !!) [1, 4, 7, 8]

unkownNumbers :: [Int]
unkownNumbers = [0, 2, 3, 5, 6, 9]

unknownStrings :: [String]
unknownStrings = map (numStrings !!) unkownNumbers

diffKnownUnknown :: [[Int]]
diffKnownUnknown = map (diffKU knownStrings) unknownStrings

diffKU :: [String] -> String -> [Int]
diffKU base us = map (\ks -> length $ ks \\ us) base

toTuple :: [b] -> (b, b)
toTuple [a, b] = (a, b)
toTuple _ = error "unsafe error"