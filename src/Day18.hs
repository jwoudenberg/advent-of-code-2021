{-# LANGUAGE TupleSections #-}

module Day18 (main) where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified System.IO
import qualified Text.ParserCombinators.ReadPrec as P
import Text.Read (readPrec)

main :: IO ()
main = do
  input <- parse
  tail input
    & foldl' add (head input)
    & magnitude
    & print

add :: SnailNo -> SnailNo -> SnailNo
add a b =
  reduce (Pair a b)

magnitude :: SnailNo -> Int
magnitude (Regular x) = x
magnitude (Pair x y) = 3 * (magnitude x) + 2 * (magnitude y)

data SnailNo
  = Regular Int
  | Pair SnailNo SnailNo

instance Show SnailNo where
  show (Regular n) = show n
  show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"

instance Read SnailNo where
  readPrec =
    P.choice
      [ Regular <$> readPrec,
        do
          '[' <- P.get
          a <- readPrec
          ',' <- P.get
          b <- readPrec
          ']' <- P.get
          pure (Pair a b)
      ]

isPair :: SnailNo -> Bool
isPair (Pair _ _) = True
isPair (Regular _) = False

reduce :: SnailNo -> SnailNo
reduce snailNo =
  case findNestedFourDeep snailNo of
    Just zipper ->
      reduce (explode zipper)
    Nothing ->
      case findAtLeast10 snailNo of
        Just zipper ->
          reduce (split zipper)
        Nothing ->
          snailNo

findNestedFourDeep :: SnailNo -> Maybe Zipper
findNestedFourDeep no = go (toZipper no)
  where
    go zipper =
      if isPair (current zipper) && depth zipper >= 4
        then Just zipper
        else next zipper >>= (go . snd)

explode :: Zipper -> SnailNo
explode zipper =
  case current zipper of
    Pair (Regular left) (Regular right) ->
      zipper
        & mapCurrent (\_ -> Regular 0)
        & addToLeft left
        & addToRight right
        & fromZipper
    _ -> undefined
  where
    addToLeft :: Int -> Zipper -> Zipper
    addToLeft n zipper' =
      case prev zipper' of
        Nothing -> zipper'
        Just (goBack, zipper'') ->
          goBack $
            case current zipper'' of
              (Regular m) ->
                mapCurrent (\_ -> Regular (n + m)) zipper''
              (Pair _ _) -> addToLeft n zipper''

    addToRight :: Int -> Zipper -> Zipper
    addToRight n zipper' =
      case next zipper' of
        Nothing -> zipper'
        Just (goBack, zipper'') ->
          goBack $
            case current zipper'' of
              (Regular m) ->
                mapCurrent (\_ -> Regular (n + m)) zipper''
              (Pair _ _) -> addToRight n zipper''

findAtLeast10 :: SnailNo -> Maybe Zipper
findAtLeast10 no = go (toZipper no)
  where
    go zipper =
      case current zipper of
        Regular n | n >= 10 -> Just zipper
        _ -> next zipper >>= (go . snd)

split :: Zipper -> SnailNo
split zipper =
  fromZipper $
    mapCurrent
      ( \no -> case no of
          Pair _ _ -> undefined
          Regular n ->
            let (q, r) = n `divMod` 2
             in Pair (Regular q) (Regular (q + r))
      )
      zipper

type Zipper = (SnailNo, ZipperContext)

data ZipperContext
  = Top
  | L ZipperContext SnailNo
  | R ZipperContext SnailNo
  deriving (Show)

toZipper :: SnailNo -> Zipper
toZipper no = (no, Top)

depth :: Zipper -> Int
depth (_, context) = go context
  where
    go Top = 0
    go (L c _) = 1 + go c
    go (R c _) = 1 + go c

fromZipper :: Zipper -> SnailNo
fromZipper (no, Top) = no
fromZipper (left, L c right) = fromZipper (Pair left right, c)
fromZipper (right, R c left) = fromZipper (Pair left right, c)

mapCurrent :: (SnailNo -> SnailNo) -> Zipper -> Zipper
mapCurrent f (x, c) = (f x, c)

current :: Zipper -> SnailNo
current (no, _) = no

downLeft :: Zipper -> Maybe Zipper
downLeft (Pair left right, c) = Just (left, L c right)
downLeft (Regular _, _) = Nothing

downRight :: Zipper -> Maybe Zipper
downRight (Pair left right, c) = Just (right, R c left)
downRight (Regular _, _) = Nothing

up :: Zipper -> Maybe Zipper
up (_, Top) = Nothing
up (left, L c right) = Just (Pair left right, c)
up (right, R c left) = Just (Pair left right, c)

next :: Zipper -> Maybe (Zipper -> Zipper, Zipper)
next zipper =
  ((fromJust . up,) <$> downLeft zipper) <|> over zipper
  where
    over (_, Top) = Nothing
    over (left, L c right) = Just (switch, (right, R c left))
    over zipper' = do
      zipper'' <- up zipper'
      (goBack, zipper''') <- over zipper''
      Just (fromJust . downRight . goBack, zipper''')

switch :: Zipper -> Zipper
switch (left, L c right) = (right, R c left)
switch (right, R c left) = (left, L c right)
switch (no, Top) = (no, Top)

prev :: Zipper -> Maybe (Zipper -> Zipper, Zipper)
prev zipper =
  ((fromJust . up,) <$> downRight zipper) <|> over zipper
  where
    over (_, Top) = Nothing
    over (right, R c left) = Just (switch, (left, L c right))
    over zipper' = do
      zipper'' <- up zipper'
      (goBack, zipper''') <- over zipper''
      Just (fromJust . downLeft . goBack, zipper''')

parse :: IO [SnailNo]
parse = do
  file <- System.IO.readFile "input/day18"
  lines file
    & fmap read
    & pure
