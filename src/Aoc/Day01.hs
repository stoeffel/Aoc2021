module Aoc.Day01 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

parser :: P.Parser (List Int)
parser = P.lines P.decimal

solution1 :: List Int -> Int
solution1 = countIncreased

solution2 :: List Int -> Int
solution2 = slidingWindows >> countIncreased

countIncreased :: List Int -> Int
countIncreased lines =
  List.map2 (,) lines (List.drop 1 lines)
    |> List.foldl
      ( \(a, b) acc ->
          if a < b
            then acc + 1
            else acc
      )
      0

slidingWindows :: List Int -> List Int
slidingWindows lines =
  List.map3
    (\a b c -> a + b + c)
    lines
    (List.drop 1 lines)
    (List.drop 2 lines)
