module Aoc.Day06 (solution) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

newtype Age = Age Int
  deriving (Eq, Ord, Num)

solution1 :: List Age -> Int
solution1 = simulate 80

solution2 :: List Age -> Int
solution2 = simulate 256

simulate :: Int -> List Age -> Int
simulate n ages =
  ages
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
