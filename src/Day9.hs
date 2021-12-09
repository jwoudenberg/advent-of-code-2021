module Day9 (main) where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let terrain =
        zip [0 :: Int ..] input
          & concatMap (\(i, row) -> zipWith (\j depth -> ((i, j), depth)) [0 :: Int ..] row)
          & Map.fromList
  minima terrain
    & fmap (Set.size . basinAround terrain . fst)
    & sort
    & reverse
    & take 3
    & product
    & print

type Coord = (Int, Int)

minima :: Map.Map Coord Int -> [(Coord, Int)]
minima terrain =
  Map.toList terrain
    & fmap
      ( \(coord, val) ->
          if all ((> val) . heightAt terrain) (neighbours coord)
            then Just (coord, val)
            else Nothing
      )
    & catMaybes

heightAt :: Map.Map Coord Int -> Coord -> Int
heightAt terrain coord = Map.lookup coord terrain & fromMaybe 9

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

basinAround :: Map.Map Coord Int -> Coord -> Set.Set Coord
basinAround terrain coord =
  expand (Set.singleton coord)
  where
    expand :: Set.Set Coord -> Set.Set Coord
    expand current =
      let next =
            Set.toList current
              & concatMap neighbours
              & Set.fromList
              & Set.union current
              & Set.filter ((< 9) . heightAt terrain)
       in if next == current then current else expand next

parse :: IO [[Int]]
parse = do
  file <- System.IO.readFile "input/day9"
  lines file
    & fmap (fmap digitToInt)
    & pure
