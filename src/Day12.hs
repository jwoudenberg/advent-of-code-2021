module Day12 (main) where

import Data.Char (toLower)
import Data.Function ((&))
import Data.List (group, groupBy, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified System.IO

type Caves = Map.Map String [String]

main :: IO ()
main = do
  input <- parse
  let caves =
        input ++ fmap swap input
          & sort
          & groupBy (\(start1, _) (start2, _) -> start1 == start2)
          & fmap (\xs -> (fst (head xs), fmap snd xs))
          & Map.fromList
  paths caves
    & length
    & print

paths :: Caves -> [[String]]
paths caves = step [["start"]] []
  where
    step :: [[String]] -> [[String]] -> [[String]]
    step [] completed = completed
    step (done@("end" : _) : rest) completed = step rest (done : completed)
    step ((current : prevs) : rest) completed =
      if mayVisit current prevs
        then
          let nexts = Map.lookup current caves & fromMaybe []
           in step (fmap (: current : prevs) nexts ++ rest) completed
        else step rest completed
    step ([] : _) _ = undefined

mayVisit :: String -> [String] -> Bool
mayVisit "start" prevs = not ("start" `elem` prevs)
mayVisit "end" prevs = not ("end" `elem` prevs)
mayVisit cave _ | not (isSmallCave cave) = True
mayVisit cave prevs =
  let visitsBySmallCave =
        filter isSmallCave (cave : prevs)
          & sort
          & group
          & fmap length
   in all (<= 2) visitsBySmallCave
        && length (filter (> 1) visitsBySmallCave) <= 1

isSmallCave :: String -> Bool
isSmallCave cave = cave == fmap toLower cave

parse :: IO [(String, String)]
parse = do
  file <- System.IO.readFile "input/day12"
  lines file
    & fmap
      ( \line ->
          (takeWhile (/= '-') line, drop 1 (dropWhile (/= '-') line))
      )
    & pure
