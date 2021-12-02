module Aoc.Day1 (part1, part2) where

import Aoc.Helpers
import qualified List

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
    (,,)
    lines
    (List.drop 1 lines)
    (List.drop 2 lines)
    |> List.map (\(a, b, c) -> a + b + c)
