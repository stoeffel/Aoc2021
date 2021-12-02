module Aoc.Helpers where

intLines :: Text -> List Int
intLines input =
  Text.split "\n" input
    |> List.filterMap Text.toInt
