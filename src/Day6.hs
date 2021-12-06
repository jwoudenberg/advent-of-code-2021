module Day6 (main) where

import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO

main :: IO ()
main = do
  input <- parse
  population 80 input
    & length
    & print

population :: Int -> [Int] -> [Int]
population 0 prev = prev
population day prev = population (day - 1) (oneDayLater prev)

oneDayLater :: [Int] -> [Int]
oneDayLater fishes =
  fishes & concatMap ageFish
  where
    ageFish 0 = [6, 8]
    ageFish n = [n - 1]

parse :: IO [Int]
parse = do
  file <- Data.Text.IO.readFile "input/day6"
  T.splitOn "," file
    & fmap (read . T.unpack)
    & pure
