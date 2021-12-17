module Day17 (main) where

import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (groupBy)
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Set as Set
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
    & snd
    & maxHeight
    & print

data Target = Target
  { xMin :: Int,
    xMax :: Int,
    yMin :: Int,
    yMax :: Int
  }
  deriving (Show)

findVelocity :: Target -> (Int, Int)
findVelocity target =
  [ (x, y)
    | y <- reverse [(yMin target) .. (0 - yMin target)],
      x <- [0 .. (xMax target)]
  ]
    & fmap
      ( \(xVel, yVel) ->
          let yTicks =
                yPositions yVel
                  & takeWhile (>= yMin target)
                  & indicesInRange (yMin target) (yMax target)
                  & Set.fromList
              xTicks =
                xPositions xVel
                  & take (if Set.null yTicks then 0 else 1 + Set.findMax yTicks)
                  & indicesInRange (xMin target) (xMax target)
                  & Set.fromList
           in if Set.disjoint xTicks yTicks
                then Nothing
                else Just (xVel, yVel)
      )
    & catMaybes
    & head

maxHeight :: Int -> Int
maxHeight = go 0
  where
    go height 0 = height
    go height yVel = go (height + yVel) (yVel - 1)

yPositions :: Int -> [Int]
yPositions = go 0
  where
    go yPos yVel = yPos : go (yPos + yVel) (yVel - 1)

xPositions :: Int -> [Int]
xPositions = go 0
  where
    go xPos xVel = xPos : go (xPos + xVel) (reduceOne xVel)
    reduceOne x =
      case compare x 0 of
        EQ -> 0
        LT -> x + 1
        GT -> x - 1

indicesInRange :: Ord a => a -> a -> [a] -> [Int]
indicesInRange lower upper xs =
  zip [0 ..] xs
    & fmap (\(i, x) -> if x >= lower && x <= upper then Just i else Nothing)
    & dropWhile isNothing
    & takeWhile isJust
    & catMaybes

parse :: IO ((Int, Int), (Int, Int))
parse = do
  file <- System.IO.readFile "input/day17"
  let line = concat (lines file) -- Remove newlines
  let isDigit' c = isDigit c || c == '-'
  case groupBy (\x y -> isDigit' x == isDigit' y) line of
    ["target area: x=", x1, "..", x2, ", y=", y1, "..", y2] -> pure ((read x1, read x2), (read y1, read y2))
    _ -> undefined
