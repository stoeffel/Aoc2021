module Aoc.Bits
  ( Bits,
    parser,
    mostCommonBit,
    complement,
    reverseBit,
    multiply,
  )
where

import Aoc.Helpers (count)
import Aoc.Parser (Parser, char, many1, oneOf, (*>))
import qualified Data.Bits as Bits
import qualified List
import qualified Prelude

data Bit = Zero | One
  deriving (Eq, Ord, Show)

type Bits = List Bit

parser :: Parser Bits
parser = many1 bitParser

bitParser :: Parser Bit
bitParser = do
  oneOf
    [ char '1' *> Prelude.pure One,
      char '0' *> Prelude.pure Zero
    ]

mostCommonBit :: Bits -> Bit
mostCommonBit xs =
  if count One xs >= count Zero xs
    then One
    else Zero

complement :: Bits -> Bits
complement = List.map reverseBit

reverseBit :: Bit -> Bit
reverseBit =
  \case
    One -> Zero
    Zero -> One

bitToInt :: Bit -> Int
bitToInt =
  \case
    One -> 1
    Zero -> 0

toInt :: Bits -> Int
toInt = List.foldl (\x acc -> Bits.shiftL acc 1 + bitToInt x) 0

multiply :: Bits -> Bits -> Int
multiply x y = toInt x * toInt y
