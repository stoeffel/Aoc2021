{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Aoc.Day1
import qualified Aoc.Day10
import qualified Aoc.Day11
import qualified Aoc.Day12
import qualified Aoc.Day13
import qualified Aoc.Day14
import qualified Aoc.Day15
import qualified Aoc.Day16
import qualified Aoc.Day17
import qualified Aoc.Day18
import qualified Aoc.Day19
import qualified Aoc.Day2
import qualified Aoc.Day20
import qualified Aoc.Day21
import qualified Aoc.Day22
import qualified Aoc.Day23
import qualified Aoc.Day24
import qualified Aoc.Day25
import qualified Aoc.Day3
import qualified Aoc.Day4
import qualified Aoc.Day5
import qualified Aoc.Day6
import qualified Aoc.Day7
import qualified Aoc.Day8
import qualified Aoc.Day9
import Aoc.Helpers
import qualified Data.Text.IO
import qualified Expect
import qualified System.Directory as Directory
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
    [ mkTests Aoc.Day1.Day1,
      mkTests Aoc.Day2.Day2,
      mkTests Aoc.Day3.Day3,
      mkTests Aoc.Day4.Day4,
      mkTests Aoc.Day5.Day5,
      mkTests Aoc.Day6.Day6,
      mkTests Aoc.Day7.Day7,
      mkTests Aoc.Day8.Day8,
      mkTests Aoc.Day9.Day9,
      mkTests Aoc.Day10.Day10,
      mkTests Aoc.Day11.Day11,
      mkTests Aoc.Day12.Day12,
      mkTests Aoc.Day13.Day13,
      mkTests Aoc.Day14.Day14,
      mkTests Aoc.Day15.Day15,
      mkTests Aoc.Day16.Day16,
      mkTests Aoc.Day17.Day17,
      mkTests Aoc.Day18.Day18,
      mkTests Aoc.Day19.Day19,
      mkTests Aoc.Day20.Day20,
      mkTests Aoc.Day21.Day21,
      mkTests Aoc.Day22.Day22,
      mkTests Aoc.Day23.Day23,
      mkTests Aoc.Day24.Day24,
      mkTests Aoc.Day25.Day25
    ]

mkTests :: (Solution b) => b -> Test
mkTests solution =
  describe
    (name solution)
    [ mkTest (solution1 solution) solution "-example-part1",
      mkTest (solution1 solution) solution "-part1",
      mkTest (solution2 solution) solution "-example-part2",
      mkTest (solution2 solution) solution "-part2"
    ]

mkTest :: Solution a => (Text -> Text) -> a -> Text -> Test
mkTest run solution partX =
  test testName <| \() -> do
    let asset = Text.toList <| "test/assets/" ++ testName ++ ".txt"
    exists <- Directory.doesFileExist asset |> Expect.fromIO
    if exists
      then do
        input <- Data.Text.IO.readFile asset |> Expect.fromIO
        run input
          |> Expect.equalToContentsOf ("test/golden-results/" ++ testName ++ ".hs")
      else do
        Expect.fromIO (Data.Text.IO.writeFile asset "TODO")
        Expect.fail "No asset file found (created one)"
  where
    testName = Text.toLower (name solution) ++ partX
