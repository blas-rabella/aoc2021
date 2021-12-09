module Main where

import Data.Bits
import Data.List (transpose)

type NumOfOnes = [Int]

type BinaryString = String

main :: IO ()
main = do
  input <- lines <$> getContents
  let (gamma, epsilon) = solve1 input
  print $ gamma * epsilon
  print $ solve2 input

solve1 :: [BinaryString] -> (Int, Int)
solve1 l = (gamma, epsilon)
  where
    numElems = length l
    ones = computeOnes l
    gammaBits = map (> numElems `div` 2) ones
    gamma = toNum gammaBits
    epsilon = toNum $ map not gammaBits

solve2 :: [BinaryString] -> Int
solve2 l = oxigenReading * co2Reading
  where
    oxigenReading = toInt $ oxigen 0 l
    co2Reading = toInt $ co2 0 l
    toInt :: BinaryString -> Int
    toInt = toNum . map (== '1')

oxigen :: Int -> [BinaryString] -> BinaryString
oxigen ix numbers = if length filtered == 1 then head filtered else oxigen (ix + 1) filtered
  where
    ones = computeOnes numbers
    thisIxOnes = ones !! ix
    thisIxZeros = length numbers - thisIxOnes
    mostCommon = if thisIxZeros <= ones !! ix then '1' else '0'
    filtered = filter (\n -> mostCommon == n !! ix) numbers

co2 :: Int -> [BinaryString] -> BinaryString
co2 ix numbers = if length filtered == 1 then head filtered else co2 (ix + 1) filtered
  where
    ones = computeOnes numbers
    thisIxOnes = ones !! ix
    thisIxZeros = length numbers - thisIxOnes
    mostCommon = if thisIxOnes >= thisIxZeros then '0' else '1'
    filtered = filter (\n -> mostCommon == n !! ix) numbers

computeOnes :: [BinaryString] -> NumOfOnes
computeOnes = map (sum . map parse) . transpose
  where
    parse '1' = 1
    parse '0' = 0
    parse _ = error "unexpected input"

toNum :: [Bool] -> Int -- would work with
toNum l = foldr setBit' zeroBits (zip (reverse l) [0 .. length l])
  where
    setBit' :: (Bool, Int) -> Int -> Int
    setBit' (False, _) a = a
    setBit' (True, i) a = setBit a i
