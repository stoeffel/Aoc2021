module Aoc.Day03 (Day03 (..)) where

import qualified Aoc.Bits as Bits
import Aoc.Helpers (Solution (..), lines, unsafeParse)
import Control.Applicative ((*>))
import Data.List (transpose)
import qualified List

data Day03 = Day03

instance Solution Day03 where
  solution1 _ = part1 >> Debug.toString
  solution2 _ = part2 >> Debug.toString

part1 :: Text -> Int
part1 input =
  Bits.multiply gammaRate epsilonRate
  where
    epsilonRate = Bits.complement gammaRate
    gammaRate =
      unsafeParse (lines Bits.parser) input
        |> transpose
        |> List.map Bits.mostCommonBit

part2 :: Text -> Int
part2 input =
  Bits.multiply oxygenRate co2ScrubberRate
  where
    bits = unsafeParse (lines Bits.parser) input
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
