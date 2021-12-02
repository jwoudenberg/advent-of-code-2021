module Day2 (main) where

import qualified System.IO

main :: IO ()
main = do
  input <- parse
  let start = (0, 0, 0)
  let (_, hor, ver) = foldl go start input
  print (hor * ver)

go :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
go (aim, hor, ver) (instruction, amount) =
  case instruction of
    "up" -> (aim - amount, hor, ver)
    "down" -> (aim + amount, hor, ver)
    "forward" -> (aim, hor + amount, ver + (aim * amount))
    _ -> undefined

parse :: IO [(String, Int)]
parse = do
  file <- System.IO.readFile "day2.input"
  pure $ parseLine <$> lines file

parseLine :: String -> (String, Int)
parseLine line =
  case words line of
    [instruction, amount] -> (instruction, read amount)
    _ -> undefined
