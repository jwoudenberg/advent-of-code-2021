module Day5 (main) where

import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (group, groupBy, sort)
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  input
    & concatMap (uncurry coordsInLine)
    & sort
    & group
    & filter (\coord -> length coord >= 2)
    & length
    & print

coordsInLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
coordsInLine (x1, y1) (x2, y2) =
  if (x1 == x2)
    then zip (repeat x1) (range y1 y2)
    else
      if (y1 == y2)
        then zip (range x1 x2) (repeat y1)
        else []

range :: Int -> Int -> [Int]
range x y = if x < y then [x .. y] else [y .. x]

parse :: IO [((Int, Int), (Int, Int))]
parse = do
  file <- System.IO.readFile "input/day5"
  lines file
    & fmap parseLine
    & pure

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line =
  case groupBy (\x y -> isDigit x == isDigit y) line of
    [x1, ",", y1, " -> ", x2, ",", y2] -> ((read x1, read y1), (read x2, read y2))
    _ -> undefined
