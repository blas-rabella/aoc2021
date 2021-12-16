module Main where

import Debug.Trace (trace)
import Data.List (sort)

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ solve1 input
  print $ solve2 input

solve1 :: [[Char]] -> Int
solve1 input = sum . map isClosing $ errors
  where
    errors :: [Char]
    errors = map (head . snd) . filter (not . fst) $ map (checkLine []) input

solve2 :: [[Char]] -> Int
solve2 input = aps !! (length aps `div` 2)
  where
    ps = points input
    aps = sort $ autocompletPoints ps 

points :: [String] -> [[Int]]
points = map ( map (points2 . invert) ) . rests


autocompletPoints :: [[Int]] -> [Int]
autocompletPoints = map (foldl (\ z a -> (z*5) + a) 0) 

rests :: [String] -> [String]
rests = map snd . filter fst . map (checkLine [])

checkLine :: [Char] -> [Char] -> (Bool, [Char])
checkLine stack [] = (True, stack)
checkLine stack (c:rest)
  | isClosing c == 0 = checkLine (c:stack) rest
  | otherwise = if isReverse (head stack) c then checkLine (tail stack) rest else (False, [c])



isReverse '<' '>' = True
isReverse '(' ')' = True
isReverse '[' ']' = True
isReverse '{' '}' = True
isReverse _ _ = False

isClosing :: Char -> Int
isClosing '>' = 25137
isClosing ')' = 3
isClosing ']' = 57
isClosing '}' = 1197
isClosing _ = 0

invert :: Char -> Char
invert '<' = '>'
invert '(' = ')'
invert '[' = ']'
invert '{' = '}'
invert a = a

points2 :: Num p => Char -> p
points2 ')' = 1
points2 ']' = 2
points2 '}' = 3
points2 '>' = 4
points2 _ = 0
