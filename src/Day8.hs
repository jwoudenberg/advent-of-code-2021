module Day8 (main) where

import Data.Function ((&))
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  input
    & concatMap snd
    & filter (\segment -> length segment `elem` [2, 3, 4, 7])
    & length
    & print

parse :: IO [([String], [String])]
parse = do
  file <- System.IO.readFile "input/day8"
  lines file
    & fmap parseLine
    & pure

parseLine :: String -> ([String], [String])
parseLine line =
  case words line of
    [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, "|", o0, o1, o2, o3] ->
      ([i0, i1, i2, i3, i4, i5, i6, i7, i8, i9], [o0, o1, o2, o3])
    _ -> undefined
