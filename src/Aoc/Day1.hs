module Aoc.Day1 (Day1 (..)) where

import Aoc.Helpers (Solution (..), intLines)
import qualified List

data Day1 = Day1

instance Solution Day1 where
  solution1 _ = part1 >> Debug.toString
  solution2 _ = part2 >> Debug.toString

part1 :: Text -> Int
part1 input =
  intLines input
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

part2 :: Text -> Int
part2 input =
  intLines input
    |> slidingWindows
    |> countIncreased

slidingWindows :: List Int -> List Int
slidingWindows lines =
  List.map3
    (\a b c -> a + b + c)
    lines
    (List.drop 1 lines)
    (List.drop 2 lines)
