module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- map read . splitOn "," <$> getContents
  print $ solve1 input
  print $ solve2 input

solve1 :: [Integer] -> Integer
solve1 xs = cost1 i xs
  where
    i = sort xs !! (length xs `div` 2)

solve2 :: [Integer] -> Integer
solve2 xs = cost2 i xs
  where
    i = sum xs `div` toInteger (length xs)

cost1 :: Num c => c -> [c] -> c
cost1 t = sum . map (\n -> abs (n - t))

cost2 t = sum . map f
  where
    f n = abs (n - t) * (abs (n - t) + 1) `div` 2