module Aoc.Day06 (Day06 (..)) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import Aoc.Solution (Solution (..))

data Day06 = Day06

instance Solution Day06 Int where
  solution1 _ = simulate 80
  solution2 _ = simulate 256

newtype Age = Age Int
  deriving (Eq, Ord, Num)

simulate :: Int -> Text -> Int
simulate n input =
  P.unsafeParse parser input
    |> Counter.count
    |> ageFishs n
    |> Counter.total

parser :: P.Parser (List Age)
parser = P.csv (map Age P.decimal)

ageFishs :: Int -> Counter Age -> Counter Age
ageFishs 0 fishs = fishs
ageFishs x fishs =
  Counter.foldl ageFish Counter.empty fishs
    |> ageFishs (x - 1)
  where
    ageFish :: Age -> Int -> Counter Age -> Counter Age
    ageFish 0 v acc = Counter.add 6 v (Counter.add 8 v acc)
    ageFish k v acc = Counter.add (k - 1) v acc
