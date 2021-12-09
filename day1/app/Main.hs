module Main where

main :: IO ()
main = do
  input <- map read . lines <$> getContents
  print $ solve1 input
  print $ solve2 input


solve1 :: [Int] -> Int
solve1 l = sum $ zipWith (\ this after -> (if after > this then 1 else 0)) l (tail l)

solve2 :: [Int] -> Int
solve2 l = sum $ zipWith (\ this after -> (if after > this then 1 else 0)) windowed (tail windowed)
  where windowed = zipWith3 (\a b c -> a + b + c) l (tail l) (tail $ tail l)

