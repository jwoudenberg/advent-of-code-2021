module Day6 (main) where

import Data.Function ((&))
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO

main :: IO ()
main = do
  input <- parse
  groupByAge input
    & population 256
    & sum
    & print

groupByAge :: [Int] -> [Int]
groupByAge fishes =
  let ageAndCount =
        fishes
          & sort
          & group
          & map (\ageCohort -> (head ageCohort, length ageCohort))
   in fmap
        (\age -> fromMaybe 0 (lookup age ageAndCount))
        [0 .. (maximum (fmap fst ageAndCount))]

population :: Int -> [Int] -> [Int]
population 0 prev = prev
population day prev = population (day - 1) (oneDayLater prev)

oneDayLater :: [Int] -> [Int]
oneDayLater [] = []
oneDayLater (age0 : otherAges) =
  zipWith
    (+)
    (otherAges ++ repeat 0)
    [0, 0, 0, 0, 0, 0, age0, 0, age0]

parse :: IO [Int]
parse = do
  file <- Data.Text.IO.readFile "input/day6"
  T.splitOn "," file
    & fmap (read . T.unpack)
    & pure
