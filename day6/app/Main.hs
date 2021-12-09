module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  fishes <- map read . splitOn "," <$> getContents
  let steps = iterate stepFishCount (buildFishCount fishes)
  print $ sum . M.elems $ (steps !! 80)
  print $ sum . M.elems $ (steps !! 256)

buildFishCount :: [Int] -> M.Map Int Int
buildFishCount = M.fromListWith (+) . (`zip` [1, 1 ..])

stepFishCount :: M.Map Int Int -> M.Map Int Int
stepFishCount oldFish = endCount
  where
    dayAfter = M.mapKeys (\k -> k -1) oldFish
    fryCount :: Maybe Int
    fryCount = M.lookup (negate 1) dayAfter
    spawnFry = M.alter (const fryCount) newFish
    restartFish Nothing = id
    restartFish (Just n) = M.alter (adding n) restart
    endCount = restartFish fryCount $ spawnFry $ M.delete (-1) dayAfter

adding :: Num b => b -> Maybe b -> Maybe b
adding n Nothing = Just n
adding n x = fmap (+ n) x

newFish = 8

restart = 6