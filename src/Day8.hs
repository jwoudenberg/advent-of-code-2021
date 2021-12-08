module Day8 (main) where

import Data.Function ((&))
import Data.List (elemIndex, find, group, sort, (\\))
import Data.Maybe (fromJust)
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  input
    & fmap
      ( \(inputs, outputs) ->
          let digits = sortByDigitRepresented inputs
           in outputs
                & fmap (\output -> fromJust $ elemIndex output digits)
                & digitsToInt
      )
    & sum
    & print

digitsToInt :: [Int] -> Int
digitsToInt xs = go 1 (reverse xs) 0
  where
    go _ [] acc = acc
    go n (y : ys) acc = go (10 * n) ys (acc + n * y)

sortByDigitRepresented :: [String] -> [String]
sortByDigitRepresented inputs =
  let segCounts = concat inputs & sort & group
      digit1 = fromJust $ find ((== 2) . length) inputs
      digit4 = fromJust $ find ((== 4) . length) inputs
      digit7 = fromJust $ find ((== 3) . length) inputs
      digit8 = fromJust $ find ((== 7) . length) inputs
      segB = segCounts & find ((== 6) . length) & fromJust & head
      segE = segCounts & find ((== 4) . length) & fromJust & head
      segF = segCounts & find ((== 9) . length) & fromJust & head
      segC = head $ digit1 \\ [segF]
      digit9 = digit8 \\ [segE]
      digit3 = digit8 \\ [segB, segE]
      digit2 = digit8 \\ [segB, segF]
      digit6 = digit8 \\ [segC]
      digit5 = digit8 \\ [segC, segE]
      digit0 = head $ inputs \\ [digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9]
   in [digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9]

parse :: IO [([String], [String])]
parse = do
  file <- System.IO.readFile "input/day8"
  lines file
    & fmap parseLine
    & pure

parseLine :: String -> ([String], [String])
parseLine line =
  case fmap sort (words line) of
    [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, "|", o0, o1, o2, o3] ->
      ([i0, i1, i2, i3, i4, i5, i6, i7, i8, i9], [o0, o1, o2, o3])
    _ -> undefined
