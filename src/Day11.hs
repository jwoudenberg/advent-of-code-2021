module Day11 (main) where

import Data.Char (digitToInt)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let jumbos =
        zip [0 :: Int ..] input
          & concatMap (\(i, row) -> zipWith (\j depth -> ((i, j), depth)) [0 :: Int ..] row)
          & Map.fromList
  countFlashes 100 jumbos
    & print

type Coord = (Int, Int)

countFlashes :: Int -> Map.Map Coord Int -> Int
countFlashes = go 0
  where
    go :: Int -> Int -> Map.Map Coord Int -> Int
    go total 0 _ = total
    go total i jumbos =
      let newJumbos = step jumbos
          flashes = Map.filter (== 0) newJumbos & Map.size
       in go (total + flashes) (i - 1) newJumbos

_printJumbos :: Map.Map Coord Int -> String
_printJumbos jumbos =
  Map.toList jumbos
    & List.groupBy (\((rowX, _), _) ((rowY, _), _) -> rowX == rowY)
    & fmap (concat . fmap (show . snd) . List.sortOn (snd . fst))
    & List.intersperse "\n"
    & concat

step :: Map.Map Coord Int -> Map.Map Coord Int
step = flash . fmap (+ 1)
  where
    flash :: Map.Map Coord Int -> Map.Map Coord Int
    flash jumbos =
      let flashCoords = Map.filter (> 9) jumbos & Map.keysSet
          flashNeighbours =
            flashCoords & Set.toList
              & List.map (\coord -> zip (neighbours coord) (repeat 1) & Map.fromList)
              & List.foldr (Map.unionWith (+)) Map.empty
       in if Set.null flashCoords
            then jumbos
            else
              jumbos
                & Map.mapWithKey
                  ( \coord energy ->
                      if Set.member coord flashCoords
                        then 0
                        else case Map.lookup coord flashNeighbours of
                          Nothing -> energy
                          Just neighbourFlashes ->
                            if energy > 0
                              then energy + neighbourFlashes
                              else energy
                  )
                & flash

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(i, j) | i <- [x - 1, x, x + 1], j <- [y - 1, y, y + 1], i /= x || j /= y]

parse :: IO [[Int]]
parse = do
  file <- System.IO.readFile "input/day11"
  lines file
    & fmap (fmap digitToInt)
    & pure
