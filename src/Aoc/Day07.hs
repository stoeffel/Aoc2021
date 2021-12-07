module Aoc.Day07 (solution) where

import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Array
import qualified Data.List

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

parser :: P.Parser (List Int)
parser = P.csv P.decimal

solution1 :: List Int -> Maybe Int
solution1 positions =
  Maybe.map
    (calcOptimisticFuel positions)
    (medianPosition positions)

medianPosition :: List Int -> Maybe Int
medianPosition positions =
  let half = List.length positions // 2
   in Data.List.sort positions
        |> Array.fromList
        |> Array.get half

calcOptimisticFuel :: List Int -> Int -> Int
calcOptimisticFuel positions median =
  positions
    |> List.map (\position -> abs (position - median))
    |> List.sum

solution2 :: List Int -> Maybe Int
solution2 positions =
  Maybe.map2 List.range (List.minimum positions) (List.maximum positions)
    |> Maybe.withDefault []
    |> List.map (calcRealFuel (Counter.count positions))
    |> List.minimum

calcRealFuel :: Counter.Counter Int -> Int -> Int
calcRealFuel positions target =
  Counter.foldl
    (\k v acc -> acc + v * partialSum (abs (k - target)))
    0
    positions

partialSum :: Int -> Int
partialSum x = x * (x + 1) // 2
