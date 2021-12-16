{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Day16 (main) where

import Data.Bits ((.&.))
import Data.Char (digitToInt)
import Data.Function ((&))
import qualified System.IO

main :: IO ()
main = do
  input <- parseInput
  let (packet, _) = run parsePacket input
  sumVersions packet 0
    & print

sumVersions :: Packet -> Int -> Int
sumVersions Literal {version} acc = acc + version
sumVersions Operator {version, packets} acc =
  foldr
    sumVersions
    (acc + version)
    packets

newtype Parser a = Parser {run :: [Bool] -> (a, [Bool])}
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser (\bits -> (x, bits))
  Parser fWrapped <*> Parser xWrapped =
    Parser
      ( \bits ->
          let (f, bits2) = fWrapped bits
              (x, bits3) = xWrapped bits2
           in (f x, bits3)
      )

instance Monad Parser where
  Parser xWrapped >>= f =
    Parser
      ( \bits ->
          let (x, bits2) = xWrapped bits
              (Parser res) = f x
           in (res bits2)
      )

data Packet
  = Literal
      { version :: Int,
        bits :: [Bool]
      }
  | Operator
      { version :: Int,
        packets :: [Packet]
      }
  deriving (Show)

parsePacket :: Parser Packet
parsePacket = do
  version <- parseInt 3
  type_ <- parseInt 3
  case type_ of
    4 -> do
      bits <- parseLiteralBits []
      pure
        Literal
          { version = version,
            bits = bits
          }
    _ -> do
      packets <- parsePackets
      pure
        Operator
          { version = version,
            packets = packets
          }

parseLiteralBits :: [Bool] -> Parser [Bool]
parseLiteralBits acc = do
  more <- parseBool
  bit1 <- parseBool
  bit2 <- parseBool
  bit3 <- parseBool
  bit4 <- parseBool
  let newAcc = (bit4 : bit3 : bit2 : bit1 : acc)
  if more
    then parseLiteralBits newAcc
    else pure (reverse newAcc)

parsePackets :: Parser [Packet]
parsePackets = do
  length_type_id <- parseBool
  if length_type_id
    then do
      numPackets <- parseInt 11
      traverse
        (\_ -> parsePacket)
        [1 .. numPackets]
    else do
      totalSize <- parseInt 15
      constrain totalSize (parseList parsePacket)

parseInt :: Int -> Parser Int
parseInt = go 0
  where
    go acc 0 = pure acc
    go acc len =
      parseBool
        >>= (\bool -> go (acc + if bool then 2 ^ (len - 1) else 0) (len - 1))

parseBool :: Parser Bool
parseBool = Parser (\bits -> (head bits, tail bits))

_toString :: [Bool] -> String
_toString = fmap (\bool -> if bool then '1' else '0')

parseList :: Parser a -> Parser [a]
parseList parser = fmap reverse (go [])
  where
    go acc = do
      done <- eof
      if done
        then pure acc
        else do
          x <- parser
          go (x : acc)

constrain :: Int -> Parser a -> Parser a
constrain len (Parser parser) =
  Parser
    ( \bits ->
        let (x, left) = parser (take len bits)
         in ( x,
              left ++ (drop len bits)
            )
    )

eof :: Parser Bool
eof = Parser (\bits -> (null bits, bits))

parseInput :: IO [Bool]
parseInput = do
  file <- System.IO.readFile "input/day16"
  file
    & lines
    & concat
    & foldr
      ( \hex write ->
          let int = digitToInt hex
           in (int .&. 8 > 0) : (int .&. 4 > 0) : (int .&. 2 > 0) : (int .&. 1 > 0) : write
      )
      []
    & pure
