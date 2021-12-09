module Main where

main :: IO ()
main = do
  input <- map (parse . words) . lines <$> getContents
  print $ solve1 input
  print $ solve2 input

data Instruction = Forward Int | Up Int | Down Int deriving (Eq, Show)

-- This would be nicer with a parser...
parse :: [String] -> Instruction
parse ["forward", n] = Forward (read n)
parse ["up", n] = Up (read n)
parse ["down", n] = Down (read n)
parse _ = error "Non expected input"

solve1 :: [Instruction] -> Int
solve1 = mulTuple . foldl interpret (0, 0)
  where
    mulTuple (a, b) = a * b
    interpret (x, y) (Up n) = (x, y - n)
    interpret (x, y) (Down n) = (x, y + n)
    interpret (x, y) (Forward n) = (x + n, y)

solve2 :: [Instruction] -> Int
solve2 = mulTuple . foldl interpret (0, 0, 0)
  where
    mulTuple (a, b, _) = a * b
    interpret (x, y, aim) (Up n) = (x, y, aim - n)
    interpret (x, y, aim) (Down n) = (x, y, aim + n)
    interpret (x, y, aim) (Forward n) = (x + n, y + aim * n, aim)
