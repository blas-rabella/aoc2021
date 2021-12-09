{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.ST (ST, runST)
import Data.Attoparsec.Text
  ( Parser,
    char,
    decimal,
    endOfLine,
    many1,
    parseOnly,
    string,
  )
import Data.Massiv.Array (concatM)
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array as MA
import qualified Data.Massiv.Array.Mutable as MA
import qualified Data.Text as T
import Debug.Trace (trace)

main :: IO ()
main = do
  input <- T.pack <$> getContents
  print . solve $ parseOnly parseSegments input

data Segment = Segment {ax :: Int, ay :: Int, bx :: Int, by :: Int} deriving (Show, Eq)

type Board = A.Array A.P A.Ix2 Int

type MutBoard s = MA.MArray s A.P A.Ix2 Int

solve :: Either String [Segment] -> (Int, Int)
solve (Left _) = error "unexpected parsing input"
solve (Right segments) = (solve1 segments, solve2 segments)

solve1 :: [Segment] -> Int
solve1 segments = length . filter (> 1) $ A.toList markedBoard
  where
    ixs = concatMap genIxs $ filter (not . isDiagonal) segments
    (x, y) = boundingBox segments
    markedBoard = markBoard x y ixs

solve2 :: [Segment] -> Int
solve2 segments = length . filter (> 1) $ A.toList markedBoard
  where
    ixs = concatMap genIxs segments
    (x, y) = boundingBox segments
    markedBoard = markBoard x y ixs

markBoard :: Int -> Int -> [MA.Ix2] -> MA.Array MA.P MA.Ix2 Int
markBoard x y ixs = runST $ do
  board <- MA.newMArray (A.Sz2 x y) 0
  mark board ixs
  MA.freezeS board

genIxs :: Segment -> [MA.Ix2]
genIxs s@Segment {..}
  | isVertical s = genVerticalIx s
  | isHorizontal s = genHorizontalIx s
  | otherwise = zipWith A.Ix2 (range ax bx) (range ay by)

isDiagonal :: Segment -> Bool
isDiagonal s = not (isHorizontal s) && not (isVertical s)

isHorizontal :: Segment -> Bool
isHorizontal Segment {..} = ay == by

isVertical :: Segment -> Bool
isVertical Segment {..} = ax == bx

boundingBox :: [Segment] -> (Int, Int)
boundingBox segments = (maximumX + 1, maximumY + 1)
  where
    maxX Segment {..} = max ax bx
    maxY Segment {..} = max ay by
    maximumX = maximum . map maxX $ segments
    maximumY = maximum . map maxY $ segments

genVerticalIx :: Segment -> [A.Ix2]
genVerticalIx Segment {..} = map (A.Ix2 ax) $ range ay by

genHorizontalIx :: Segment -> [A.Ix2]
genHorizontalIx Segment {..} = map (`A.Ix2` ay) $ range ax bx

mark :: MutBoard s -> [A.Ix2] -> ST s ()
mark m = mapM_ (MA.modify_ m (return . (+ 1)))

range :: Int -> Int -> [Int]
range a b
  | a > b = reverse [b .. a]
  | otherwise = [a .. b]

-- Parsers

parseSegments :: Parser [Segment]
parseSegments = many1 $ parseSegment <* endOfLine

parseSegment :: Parser Segment
parseSegment = do
  ax <- decimal
  char ','
  ay <- decimal
  string " -> "
  bx <- decimal
  char ','
  by <- decimal
  return $ Segment {..}