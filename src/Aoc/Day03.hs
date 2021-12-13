module Aoc.Day03 (solution) where

import qualified Aoc.Bits as Bits
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Data.List (transpose)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser (List Bits.Bits)
parser = P.lines Bits.parser

solution1 :: List Bits.Bits -> Int
solution1 bits =
  Bits.multiply gammaRate epsilonRate
  where
    epsilonRate = Bits.complement gammaRate
    gammaRate =
      transpose bits
        |> List.map Bits.mostCommonBit

solution2 :: List Bits.Bits -> Int
solution2 bits =
  Bits.multiply oxygenRate co2ScrubberRate
  where
    oxygenRate = bitsCriteria Bits.mostCommonBit 0 bits
    co2ScrubberRate = bitsCriteria (Bits.mostCommonBit >> Bits.reverseBit) 0 bits

bitsCriteria :: Eq a => (List a -> a) -> Int -> List (List a) -> List a
bitsCriteria _ _ [] = []
bitsCriteria _ _ [x] = x
bitsCriteria cond i xs =
  List.filter (\x -> List.head (List.drop i x) == oneOrZero) xs
    |> bitsCriteria cond (i + 1)
  where
    oneOrZero =
      xs
        |> transpose
        |> List.drop i
        |> List.head
        |> Maybe.map cond
