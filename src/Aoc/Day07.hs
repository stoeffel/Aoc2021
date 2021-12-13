module Aoc.Day07 (solution) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Array
import qualified Data.List

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2, S.display = Debug.toString}

parser :: P.Parser (List Int)
parser = P.csv P.decimal

solution1 :: List Int -> Maybe Int
solution1 = minimalFuel abs

solution2 :: List Int -> Maybe Int
solution2 = minimalFuel (abs >> partialSum)

minimalFuel :: (Int -> Int) -> List Int -> Maybe Int
minimalFuel toFuel positions =
  Maybe.map2 List.range (List.minimum positions) (List.maximum positions)
    |> Maybe.withDefault []
    |> List.map (calcFuel toFuel (Counter.fromList positions))
    |> List.minimum

calcFuel :: (Int -> Int) -> Counter Int -> Int -> Int
calcFuel toFuel positions target =
  Counter.foldl
    (\k v acc -> acc + v * toFuel (k - target))
    0
    positions

partialSum :: Int -> Int
partialSum x = x * (x + 1) // 2
