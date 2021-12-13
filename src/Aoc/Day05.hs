module Aoc.Day05 (solution) where

import qualified Aoc.Counter as Counter
import qualified Aoc.Line as Line
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2, S.display = Debug.toString}

parser :: P.Parser (List Line.Line)
parser = P.lines Line.parser

solution1 :: List Line.Line -> Int
solution1 lines =
  lines
    |> List.filter (\line -> Line.orientation line /= Line.Diagonal)
    |> countIntersections

solution2 :: List Line.Line -> Int
solution2 lines = countIntersections lines

countIntersections :: List Line.Line -> Int
countIntersections lines =
  lines
    |> Line.intersections
    |> Counter.toList
    |> List.filter (\(_, a) -> a > 1)
    |> List.length
