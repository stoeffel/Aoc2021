module Aoc.Bits
  ( Bits,
    Bit (..),
    parser,
    mostCommonBit,
    complement,
    reverseBit,
    multiply,
    toInt,
  )
where

import qualified Aoc.Counter as Counter
import Aoc.Parser ((*>))
import qualified Aoc.Parser as P
import qualified Data.Bits as Bits
import Prelude (pure)

data Bit = Zero | One
  deriving (Eq, Ord, Show)

type Bits = List Bit

parser :: P.Parser Bits
parser = P.many1 bitParser
  where
    bitParser :: P.Parser Bit
    bitParser = do
      P.oneOf
        [ P.char '1' *> pure One,
          P.char '0' *> pure Zero
        ]

mostCommonBit :: Bits -> Bit
mostCommonBit xs =
  case Counter.max (Counter.fromList xs) of
    Just (x, _) -> x
    Nothing -> One

complement :: Bits -> Bits
complement = List.map reverseBit

reverseBit :: Bit -> Bit
reverseBit = \case
  One -> Zero
  Zero -> One

bitToInt :: Bit -> Int
bitToInt = \case
  One -> 1
  Zero -> 0

toInt :: Bits -> Int
toInt = List.foldl (\x acc -> Bits.shiftL acc 1 + bitToInt x) 0

multiply :: Bits -> Bits -> Int
multiply x y = toInt x * toInt y
