module Main where

-- With Sets and arrays it would be nicer, theoretically faster but this is good enough
import Data.Char (digitToInt)
import Data.List (nub, sortOn)
import qualified GHC.Exts as Data.Ord

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> getContents
  print $ solve1 input
  print $ solve2 input

solve1 :: Board -> Int
solve1 board = sum (map (\(i, j) -> board !! i !! j + 1) filtered)
  where
    filtered = findMinimums board

solve2 :: Board -> Int
solve2 board = product . map length . take 3 . sortOn (Data.Ord.Down . length) . map (\ix -> expand board [] [ix]) $ findMinimums board

findMinimums :: [[Int]] -> [(Int, Int)]
findMinimums board = filter (checkSmaller board) [(i, j) | i <- [0 .. length board - 1], j <- [0 .. length (head board) -1]]

type Board = [[Int]]

generateIndex :: (Int, Int) -> Board -> [(Int, Int)]
generateIndex (x, y) board = ixs
  where
    ixs = filter isBounds [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
    n1 = length board
    n2 = length (head board)
    isBounds (a, b)
      | a < 0 || a >= n1 = False
      | b < 0 || b >= n2 = False
      | otherwise = True

checkSmaller :: Board -> (Int, Int) -> Bool
checkSmaller board (i, j) = all check ixs
  where
    ixs = generateIndex (i, j) board
    check (k, l) = board !! i !! j < board !! k !! l

expandNext :: Board -> (Int, Int) -> [(Int, Int)]
expandNext board ix = ixs
  where
    ixs = filter (\(i, j) -> board !! i !! j /= 9) $ generateIndex ix board

expand :: Board -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
expand board seen [] = seen
expand board seen (initial : toSee) = expand board (initial : seen) (nub $ toSee ++ next)
  where
    next = filter (`notElem` seen) $ expandNext board initial
