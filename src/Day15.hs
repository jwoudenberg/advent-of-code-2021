module Day15 (main) where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let terrain =
        zip [0 :: Int ..] input
          & concatMap
            ( \(i, row) ->
                zipWith
                  ( \j depth ->
                      ( ((i, j)),
                        depth
                      )
                  )
                  [0 :: Int ..]
                  row
            )
          & Map.fromList
  let (goal, _) = Map.findMax terrain
  let prices =
        terrain
          & Map.toList
          & fmap (\(coord, _) -> (if coord == (0, 0) then 0 else maxBound, coord))
          & Set.fromList
  goDijkstra goal terrain prices
    & print

goDijkstra :: Coord -> Terrain -> Prices -> Int
goDijkstra goal terrain prices =
  let ((price, coord), newPrices) = Set.deleteFindMin prices
   in if coord == goal
        then price
        else case Map.lookup coord terrain of
          Nothing -> goDijkstra goal terrain newPrices
          Just _ ->
            neighbours coord
              & foldl'
                ( \prices' coord' ->
                    case Map.lookup coord' terrain of
                      Nothing -> prices'
                      Just extraPrice -> Set.insert ((price + extraPrice), coord') prices'
                )
                newPrices
              & goDijkstra goal (Map.delete coord terrain)

type Coord = (Int, Int)

type Terrain = Map.Map Coord Int

type Prices = Set.Set (Int, Coord)

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

parse :: IO [[Int]]
parse = do
  file <- System.IO.readFile "input/day15"
  lines file
    & fmap (fmap digitToInt)
    & pure
