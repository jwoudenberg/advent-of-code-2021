module Day13 (main) where

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO

type Coord = (Int, Int)

main :: IO ()
main = do
  (coords, folds) <- parse
  fold (head folds) (Set.fromList coords)
    & Set.size
    & print

fold :: (Char, Int) -> Set.Set Coord -> Set.Set Coord
fold ('x', col) coords =
  coords
    & Set.toList
    & mapMaybe
      ( \(x, y) ->
          case compare x col of
            LT -> Just (x, y)
            GT -> Just (2 * col - x, y)
            EQ -> Nothing
      )
    & Set.fromList
fold ('y', row) coords =
  coords
    & Set.toList
    & mapMaybe
      ( \(x, y) ->
          case compare y row of
            LT -> Just (x, y)
            GT -> Just (x, 2 * row - y)
            EQ -> Nothing
      )
    & Set.fromList
fold _ _ = undefined

parse :: IO ([Coord], [(Char, Int)])
parse = do
  file <- Data.Text.IO.readFile "input/day13"
  case T.splitOn "\n\n" file of
    [coords, folds] ->
      pure
        ( T.lines coords
            & fmap
              ( \line ->
                  case T.splitOn "," line of
                    [x, y] -> (read (T.unpack x), read (T.unpack y))
                    _ -> undefined
              ),
          T.lines folds
            & fmap
              ( \line ->
                  case T.splitOn "=" <$> T.stripPrefix "fold along " line of
                    Just [char, coord] -> (head (T.unpack char), read (T.unpack coord))
                    _ -> undefined
              )
        )
    _ -> undefined
