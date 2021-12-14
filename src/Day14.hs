module Day14 (main) where

import Data.Function ((&))
import Data.List (group, sort)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO

main :: IO ()
main = do
  (template, rules_) <- parse
  let rules = Map.fromList (rules_)
  steps 10 rules template
    & sort
    & group
    & fmap length
    & sort
    & (\res -> last res - head res)
    & print

type Rules = Map.Map (Char, Char) Char

steps :: Int -> Rules -> String -> String
steps 0 _ input = input
steps n rules input =
  zip input (tail input)
    & foldr insert ""
    & (:) (head input)
    & steps (n - 1) rules
  where
    insert :: (Char, Char) -> [Char] -> [Char]
    insert pair@(_, after) acc =
      case Map.lookup pair rules of
        Nothing -> after : acc
        Just new -> new : after : acc

parse :: IO (String, [((Char, Char), Char)])
parse = do
  file <- Data.Text.IO.readFile "input/day14"
  case T.splitOn "\n\n" file of
    [start, rules] ->
      pure
        ( T.unpack start,
          T.lines rules
            & fmap
              ( \line ->
                  case fmap T.unpack (T.splitOn " -> " line) of
                    [[a, b], [c]] -> ((a, b), c)
                    _ -> undefined
              )
        )
    _ -> undefined
