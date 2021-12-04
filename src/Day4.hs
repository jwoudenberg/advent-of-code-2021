module Day4 (main) where

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO

main :: IO ()
main = do
  (numbers, boards) <- parse
  let drawnNumbers = fmap (Set.fromList . flip take numbers) [1 ..]
  let (winningNumbers, winningBoard) =
        (,) <$> drawnNumbers <*> (fmap toBoard boards)
          & List.find (uncurry didWin)
          & Maybe.fromJust
  let remainingNumbers =
        Set.unions (asRows winningBoard)
          & (`Set.difference` winningNumbers)
  let score =
        Set.foldr (+) 0 remainingNumbers
          * (numbers !! (Set.size winningNumbers - 1))
  print score

parse :: IO ([Int], [[[Int]]])
parse = do
  file <- Data.Text.IO.readFile "input/day4"
  case T.splitOn "\n\n" file of
    [] -> undefined
    (numbers : boards) -> do
      pure
        ( T.splitOn "," numbers & fmap (read . T.unpack),
          fmap (fmap (fmap (read . T.unpack) . T.words) . T.lines) boards
        )

data Board = Board
  { asRows :: [Set.Set Int],
    asCols :: [Set.Set Int]
  }
  deriving (Show)

toBoard :: [[Int]] -> Board
toBoard rows =
  Board
    { asRows = fmap Set.fromList rows,
      asCols = fmap Set.fromList (List.transpose rows)
    }

didWin :: Set.Set Int -> Board -> Bool
didWin numbers board =
  List.any
    (`Set.isSubsetOf` numbers)
    (asRows board ++ asCols board)
