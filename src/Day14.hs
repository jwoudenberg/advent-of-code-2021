module Day14 (main) where

import Data.Function ((&))
import Data.List (group, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO

main :: IO ()
main = do
  (template, rules_) <- parse
  let rules = Map.fromList (rules_)
  let pairs =
        zip template (tail template)
          & sort
          & group
          & fmap (\g -> (head g, length g))
          & Map.fromList
  steps 40 rules pairs
    & Map.toList
    & concatMap (\((a, b), count) -> [Map.singleton a count, Map.singleton b count])
    -- All elements are in two pairs, except the first and the last.
    & (Map.singleton (head template) 1 :)
    & (Map.singleton (last template) 1 :)
    & foldr (Map.unionWith (+)) mempty
    & fmap (\n -> n `div` 2)
    & Map.elems
    & sort
    & (\res -> (last res - head res))
    & print

type Rules = Map.Map (Char, Char) Char

type Pairs = Map.Map (Char, Char) Int

steps :: Int -> Rules -> Pairs -> Pairs
steps 0 _ input = input
steps n rules input =
  foldr insert mempty (Map.toList input)
    & steps (n - 1) rules
  where
    insert :: ((Char, Char), Int) -> Pairs -> Pairs
    insert (pair@(before, after), count) pairs =
      case Map.lookup pair rules of
        Nothing -> pairs
        Just new ->
          pairs
            & Map.alter (Just . (+ count) . fromMaybe 0) (before, new)
            & Map.alter (Just . (+ count) . fromMaybe 0) (new, after)

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
