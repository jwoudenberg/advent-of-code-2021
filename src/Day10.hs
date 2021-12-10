module Day10 (main) where

import Data.Function ((&))
import Data.List (sort)
import qualified System.IO

main :: IO ()
main = do
  input <- parse
  input
    & fmap completeScore
    & filter (> 0)
    & mean
    & print

completeScore :: String -> Int
completeScore line = go line ""
  where
    go :: String -> String -> Int
    -- End of input
    go [] opened = closeAll opened
    -- Legal closing of a chunk
    go (')' : rest) ('(' : opened) = go rest opened
    go (']' : rest) ('[' : opened) = go rest opened
    go ('}' : rest) ('{' : opened) = go rest opened
    go ('>' : rest) ('<' : opened) = go rest opened
    -- Ilegal closing of a chunk
    go (')' : _) _ = 0
    go (']' : _) _ = 0
    go ('}' : _) _ = 0
    go ('>' : _) _ = 0
    -- Opening a chunk
    go (opener : rest) opened = go rest (opener : opened)

closeAll :: String -> Int
closeAll = go 0
  where
    go :: Int -> String -> Int
    go score [] = score
    go score ('(' : rest) = go (1 + 5 * score) rest
    go score ('[' : rest) = go (2 + 5 * score) rest
    go score ('{' : rest) = go (3 + 5 * score) rest
    go score ('<' : rest) = go (4 + 5 * score) rest
    go _ _ = undefined

mean :: Ord a => [a] -> a
mean xs =
  sort xs !! (length xs `div` 2)

parse :: IO [String]
parse = do
  file <- System.IO.readFile "input/day10"
  lines file
    & pure
