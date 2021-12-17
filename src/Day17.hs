module Day17 (main) where

import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (groupBy)
import qualified System.IO

main :: IO ()
main = do
  ((x1, x2), (y1, y2)) <- parse
  let target =
        Target
          { xMin = min x1 x2,
            xMax = max x1 x2,
            yMin = min y1 y2,
            yMax = max y1 y2
          }
  findVelocity target
    & print

data Target = Target
  { xMin :: Int,
    xMax :: Int,
    yMin :: Int,
    yMax :: Int
  }
  deriving (Show)

findVelocity :: Target -> (Int, Int)
findVelocity = undefined

parse :: IO ((Int, Int), (Int, Int))
parse = do
  file <- System.IO.readFile "input/day17"
  let line = concat (lines file) -- Remove newlines
  let isDigit' c = isDigit c || c == '-'
  case groupBy (\x y -> isDigit' x == isDigit' y) line of
    ["target area: x=", x1, "..", x2, ", y=", y1, "..", y2] -> pure ((read x1, read x2), (read y1, read y2))
    _ -> undefined
