module Day9 (main) where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (zipWith5)
import Data.Maybe (catMaybes)
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let width = length (head input)
  let datapoints = concat input
  let borderVal = 100
  let leftNeighbours = drop 1 datapoints ++ repeat borderVal
  let rightNeighbours = borderVal : datapoints
  let topNeighbours = drop width datapoints ++ repeat borderVal
  let bottomNeighbours = replicate width borderVal ++ datapoints
  zipWith5 dangerVal datapoints leftNeighbours topNeighbours rightNeighbours bottomNeighbours
    & catMaybes
    & sum
    & print

dangerVal :: Int -> Int -> Int -> Int -> Int -> Maybe Int
dangerVal point left top right bottom =
  if point < left && point < top && point < right && point < bottom
    then Just (point + 1)
    else Nothing

parse :: IO [[Int]]
parse = do
  file <- System.IO.readFile "input/day9"
  lines file
    & fmap (fmap digitToInt)
    & pure
