module Day1 (main) where

import Data.Function ((&))
import qualified System.IO

main :: IO ()
main = do
  file <- System.IO.readFile "input/day1"
  let depths :: [Int] = read <$> lines file
  let windows =
        depths
          & zipWith (+) (drop 1 depths)
          & zipWith (+) (drop 2 depths)
  zipWith (-) windows (drop 1 windows)
    & filter (< 0)
    & length
    & print
