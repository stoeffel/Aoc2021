{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Aoc.Day1
import qualified Aoc.Day2
import qualified Data.Text.IO
import qualified Expect
import Test (Test, describe, test)
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = do
  Test.run tests

tests :: Test
tests =
  describe
    "AoC21"
    [ -- Day1
      mkTest Aoc.Day1.part1 "day1" "example-part1",
      mkTest Aoc.Day1.part1 "day1" "part1",
      mkTest Aoc.Day1.part2 "day1" "example-part2",
      mkTest Aoc.Day1.part2 "day1" "part2",
      -- Day2
      mkTest Aoc.Day2.part1 "day2" "example-part1",
      mkTest Aoc.Day2.part1 "day2" "part1",
      mkTest Aoc.Day2.part2 "day2" "example-part2",
      mkTest Aoc.Day2.part2 "day2" "part2"
    ]

mkTest :: Show a => (Text -> a) -> Text -> Text -> Test
mkTest run dayX partX =
  test (dayX ++ "-" ++ partX) <| \() -> do
    input <-
      Data.Text.IO.readFile (Text.toList <| "test/assets/" ++ dayX ++ "-" ++ partX ++ ".txt")
        |> Expect.fromIO
    run input
      |> Debug.toString
      |> Expect.equalToContentsOf ("test/golden-results/" ++ dayX ++ "-" ++ partX ++ ".hs")
