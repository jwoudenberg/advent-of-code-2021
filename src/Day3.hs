module Day3 (main) where

import Data.Function ((&))
import Data.List (foldl', groupBy, sortOn)
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let oxygenGenerator =
        findRating last 0 input
          & toDec
  let co2Scrubber =
        findRating head 0 input
          & toDec
  print (oxygenGenerator * co2Scrubber)

findRating :: Ord a => ([[[a]]] -> [[a]]) -> Int -> [[a]] -> [a]
findRating _ _ [row] = row
findRating pick index rows =
  rows
    & groupOn (!! index)
    & sortOn length
    & pick
    & findRating pick (index + 1)

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f xs =
  sortOn f xs
    & groupBy (\x y -> f x == f y)

toDec :: [Bool] -> Int
toDec = foldl' (\acc x -> acc * 2 + (if x then 1 else 0)) 0

parse :: IO [[Bool]]
parse = do
  file <- System.IO.readFile "input/day3"
  lines file
    & fmap (fmap (== '1'))
    & pure
