module Day1 (main) where

import Data.Function ((&))
import qualified System.IO

main :: IO ()
main = do
  file <- System.IO.readFile "day1.input"
  let depths :: [Int] = read <$> lines file
  zipWith (-) depths (drop 1 depths)
    & filter (< 0)
    & length
    & print
