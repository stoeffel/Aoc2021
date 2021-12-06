module Aoc.Day01 (Day01 (..)) where

import qualified Aoc.Parser as P
import Aoc.Solution (Solution (..))

data Day01 = Day01

instance Solution Day01 Int where
  solution1 _ = part1
  solution2 _ = part2

part1 :: Text -> Int
part1 input =
  P.unsafeParse (P.lines P.decimal) input
    |> countIncreased

part2 :: Text -> Int
part2 input =
  P.unsafeParse (P.lines P.decimal) input
    |> slidingWindows
    |> countIncreased

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
