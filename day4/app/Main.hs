module Main where

import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Massiv.Array as A
import Debug.Trace (trace)

main :: IO ()
main = do
  (numbers, boards) <- parseProblem . lines <$> getContents
  print $ solve1 numbers boards
  print $ solve2 numbers boards

type Cell = (Int, Bool)

type Board = A.Array A.B A.Ix2 Cell

solve1 :: [Int] -> [Board] -> Int
solve1 numbers boards = trace ("value " ++ show value ++ " winning sum " ++ show winningSum ++ "\nwinning board: " ++ show winningBoard) $ value * winningSum
  where
    solutions = tail $ runGame numbers boards -- the first will be without draw
    (value, winningBoards) = head . dropWhile (\(_, b) -> not $ any isWinner b) $ zip numbers solutions
    winningBoard = head $ filter isWinner winningBoards
    winningSum = boardScore winningBoard

solve2 :: [Int] -> [Board] -> Int
solve2 numbers boards = trace ("value " ++ show n ++ " winning sum " ++ show winningSum ++ "\nwinning board: " ++ show b) $ n * winningSum
  where
    solutions = tail $ runGame numbers boards -- the first will be without draw
    (value, lastLosing) = head . dropWhile (not . oneLeftLosing)  . dropWhile (\(_, b) -> not $ any isWinner b) $ zip numbers solutions
    nextValues = tail $ dropWhile (/= value) numbers
    losingBoard = head $ filter (not . isWinner) lastLosing
    lastSolutions = tail $ runGame nextValues [losingBoard]
    (n, b) = head . dropWhile (\(_, b) -> not $ any isWinner b) $ zip nextValues lastSolutions
    winningSum = boardScore $ head b

oneLeftLosing :: (Int, [Board]) -> Bool
oneLeftLosing (_, boards) = length losing == 1
  where
    (_, losing) = partition isWinner boards

boardScore :: Board -> Int
boardScore winningBoard = sum . map fst . filter (not . snd) $ A.toList winningBoard

runGame :: [Int] -> [Board] -> [[Board]]
runGame numbers boards = scanl (\bs n -> map (markBoard n) bs) boards numbers

markCell :: Int -> Cell -> Cell
markCell a (b, True) = (b, True)
markCell a (b, False)
  | a == b = (b, True)
  | otherwise = (b, False)

markBoard :: Int -> Board -> Board
markBoard number board = markCell number <$> board

isWinner :: Board -> Bool
isWinner board = row || column
  where
    row = any (all snd) (A.outerSlices board)
    column = any (all snd) (A.innerSlices board)

-- parse
--
parseProblem :: [String] -> ([Int], [Board])
parseProblem lines = (numbers, boards)
  where
    numbers = parseDraws $ head lines
    boards = parseBoards $ tail lines

parseDraws :: String -> [Int]
parseDraws = map read . splitOn ","

parseNumbers :: String -> [Int]
parseNumbers = map read . words

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards ("" : rest) = board : parseBoards next
  where
    next = dropWhile (/= "") rest
    board = parseBoard $ takeWhile (/= "") rest
parseBoards l = error ("unexpected input" ++ show l)

parseCells :: String -> [Cell]
parseCells = flip zip (repeat False) . parseNumbers

parseBoard :: [String] -> Board
parseBoard = A.fromLists' A.Seq . map parseCells
