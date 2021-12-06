module Aoc.Day06 (Day06 (..)) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import Aoc.Helpers (Solution (..))
import qualified Aoc.Parser as P
import qualified List
import Prelude (flip)

data Day06 = Day06

instance Solution Day06 Int where
  solution1 _ = part1
  solution2 _ = part2

fishParser :: P.Parser (List Int)
fishParser = P.sepBy1 (P.decimal) (P.char ',')

part1 :: Text -> Int
part1 input =
  P.unsafeParse fishParser input
    |> Counter.count
    |> ageFishs 80
    |> Counter.total

part2 :: Text -> Int
part2 input =
  P.unsafeParse fishParser input
    |> Counter.count
    |> ageFishs 256
    |> Counter.total

ageFishs :: Int -> Counter Int -> Counter Int
ageFishs 0 fishs = fishs
ageFishs x fishs =
  Counter.foldl ageFish Counter.empty fishs
    |> ageFishs (x - 1)

ageFish :: Int -> Int -> Counter Int -> Counter Int
ageFish 0 v acc = Counter.add 6 v (Counter.add 8 v acc)
ageFish k v acc = Counter.add (k - 1) v acc
