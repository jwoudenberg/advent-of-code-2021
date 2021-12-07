module Day7 (main) where

import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO

main :: IO ()
main = do
  input <- parse
  [(minimum input) .. (maximum input)]
    & fmap (fuelCost input)
    & minimum
    & print

fuelCost :: [Int] -> Int -> Int
fuelCost crabs target =
  crabs
    & fmap (\crab -> costs !! abs (crab - target))
    & sum

costs :: [Int]
costs = go 0 0
  where
    go prev inc = (prev + inc) : go (prev + inc) (inc + 1)

parse :: IO [Int]
parse = do
  file <- Data.Text.IO.readFile "input/day7"
  T.splitOn "," file
    & fmap (read . T.unpack)
    & pure
