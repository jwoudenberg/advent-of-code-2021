module Day3 (main) where

import Data.Function ((&))
import Data.List (foldl', group, sort, sortOn, transpose)
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let binaryGamma =
        transpose input
          & fmap mostCommon
  let gamma = toDec binaryGamma
  let epsilon = toDec (fmap not binaryGamma)
  print (gamma * epsilon)

mostCommon :: Ord a => [a] -> a
mostCommon xs =
  sort xs
    & group
    & sortOn length
    & last -- biggest group
    & head

toDec :: [Bool] -> Int
toDec = foldl' (\acc x -> acc * 2 + (if x then 1 else 0)) 0

parse :: IO [[Bool]]
parse = do
  file <- System.IO.readFile "input/day3"
  lines file
    & fmap (fmap (== '1'))
    & pure
