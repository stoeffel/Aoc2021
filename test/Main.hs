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
import qualified Aoc.Parser as P
import Aoc.Solution
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
    [ mkTests Aoc.Day01.solution "Day01",
      mkTests Aoc.Day02.solution "Day02",
      mkTests Aoc.Day03.solution "Day03",
      mkTests Aoc.Day04.solution "Day04",
      mkTests Aoc.Day05.solution "Day05",
      mkTests Aoc.Day06.solution "Day06",
      mkTests Aoc.Day07.solution "Day07",
      mkTests Aoc.Day08.solution "Day08",
      mkTests Aoc.Day09.solution "Day09",
      mkTests Aoc.Day10.solution "Day10",
      mkTests Aoc.Day11.solution "Day11",
      mkTests Aoc.Day12.solution "Day12",
      mkTests Aoc.Day13.solution "Day13",
      mkTests Aoc.Day14.solution "Day14",
      mkTests Aoc.Day15.solution "Day15",
      mkTests Aoc.Day16.solution "Day16",
      mkTests Aoc.Day17.solution "Day17",
      mkTests Aoc.Day18.solution "Day18",
      mkTests Aoc.Day19.solution "Day19",
      mkTests Aoc.Day20.solution "Day20",
      mkTests Aoc.Day21.solution "Day21",
      mkTests Aoc.Day22.solution "Day22",
      mkTests Aoc.Day23.solution "Day23",
      mkTests Aoc.Day24.solution "Day24",
      mkTests Aoc.Day25.solution "Day25"
    ]

mkTests :: Solution -> Text -> Test
mkTests solution name =
  describe
    name
    [ mkTest solution name Part1 Example,
      mkTest solution name Part1 Real,
      mkTest solution name Part2 Example,
      mkTest solution name Part2 Real
    ]

data Run = Example | Real
  deriving (Show)

data Part = Part1 | Part2
  deriving (Show)

mkTest :: Solution -> Text -> Part -> Run -> Test
mkTest solution name part run =
  test (Debug.toString part ++ " " ++ Debug.toString run)
    <| \() -> mkExpectation solution name part run

mkExpectation :: Solution -> Text -> Part -> Run -> Expect.Expectation
mkExpectation Solution {parser, solution1, solution2} name part run = do
  let solutionName = Text.toLower name
  let asset =
        Text.toList <| "test/assets/" ++ case run of
          Real -> solutionName ++ ".txt"
          Example -> solutionName ++ "-example.txt"
  let golden =
        solutionName
          ++ case part of
            Part1 -> "-part1"
            Part2 -> "-part2"
          ++ case run of
            Example -> "-example"
            Real -> ""
  let run = case part of
        Part1 -> solution1
        Part2 -> solution2
  input <- Expect.fromIO (Data.Text.IO.readFile asset)
  parsed <- Expect.fromResult (P.parse parser input)
  run parsed
    |> Debug.toString
    |> Expect.equalToContentsOf ("test/golden-results/" ++ golden ++ ".hs")
