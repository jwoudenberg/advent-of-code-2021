module Day10 (main) where

import Data.Function ((&))
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  input
    & fmap failingScore
    & sum
    & print

failingScore :: String -> Int
failingScore line = go line ""
  where
    go :: String -> String -> Int
    -- End of input
    go [] _ = 0
    -- Legal closing of a chunk
    go (')' : rest) ('(' : opened) = go rest opened
    go (']' : rest) ('[' : opened) = go rest opened
    go ('}' : rest) ('{' : opened) = go rest opened
    go ('>' : rest) ('<' : opened) = go rest opened
    -- Ilegal closing of a chunk
    go (')' : _) _ = 3
    go (']' : _) _ = 57
    go ('}' : _) _ = 1197
    go ('>' : _) _ = 25137
    -- Opening a chunk
    go (opener : rest) opened = go rest (opener : opened)

parse :: IO [String]
parse = do
  file <- System.IO.readFile "input/day10"
  lines file
    & pure
