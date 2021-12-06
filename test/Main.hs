module Main (main) where

import qualified Aoc.Day01
import qualified Aoc.Day02
import qualified Aoc.Day03
import qualified Aoc.Day04
import qualified Aoc.Day05
import qualified Aoc.Day06
import qualified Aoc.Day07
import qualified Aoc.Day08
import qualified Aoc.Day09
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
import qualified Aoc.Day20
import qualified Aoc.Day21
import qualified Aoc.Day22
import qualified Aoc.Day23
import qualified Aoc.Day24
import qualified Aoc.Day25
import Aoc.Solution
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
    [ mkTests Aoc.Day01.Day01,
      mkTests Aoc.Day02.Day02,
      mkTests Aoc.Day03.Day03,
      mkTests Aoc.Day04.Day04,
      mkTests Aoc.Day05.Day05,
      mkTests Aoc.Day06.Day06,
      mkTests Aoc.Day07.Day07,
      mkTests Aoc.Day08.Day08,
      mkTests Aoc.Day09.Day09,
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

mkTests :: (Solution a b) => a -> Test
mkTests solution =
  describe
    (name solution)
    [ mkTest solution Part1 Example,
      mkTest solution Part1 Real,
      mkTest solution Part2 Example,
      mkTest solution Part2 Real
    ]

data Run = Example | Real

data Part = Part1 | Part2

mkTest :: Solution a b => a -> Part -> Run -> Test
mkTest solution partX runX =
  test testName <| \() -> do
    let asset =
          Text.toList <| "test/assets/" ++ case runX of
            Real -> solutionName ++ ".txt"
            Example -> solutionName ++ "-example.txt"
    let run = case partX of
          Part1 -> solution1
          Part2 -> solution2
    exists <- Expect.fromIO (Directory.doesFileExist asset)
    if exists
      then do
        input <- Expect.fromIO (Data.Text.IO.readFile asset)
        run solution input
          |> Debug.toString
          |> Expect.equalToContentsOf ("test/golden-results/" ++ testName ++ ".hs")
      else do
        Expect.fromIO (Data.Text.IO.writeFile asset "TODO")
        Expect.fail "No asset file found (created one)"
  where
    solutionName = Text.toLower (name solution)
    testName =
      solutionName
        ++ case partX of
          Part1 -> "-part1"
          Part2 -> "-part2"
        ++ case runX of
          Example -> "-example"
          Real -> ""
