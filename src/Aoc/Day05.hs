module Aoc.Day05 (Day05 (..)) where

import Aoc.Helpers (Solution (..))
import qualified Aoc.Line as Line
import qualified Aoc.Parser as P
import qualified Dict
import qualified List
import qualified Prelude

data Day05 = Day05

instance Solution Day05 Int where
  solution1 _ = part1
  solution2 _ = part2

part1 :: Text -> Int
part1 input =
  P.unsafeParse (P.lines Line.parser) input
    |> List.filter (\line -> Line.orientation line /= Line.Diagonal)
    |> countIntersections

part2 :: Text -> Int
part2 input =
  P.unsafeParse (P.lines Line.parser) input
    |> countIntersections

countIntersections :: List Line.Line -> Int
countIntersections lines =
  lines
    |> Line.intersections
    |> Dict.values
    |> List.filter (> 1)
    |> List.length
