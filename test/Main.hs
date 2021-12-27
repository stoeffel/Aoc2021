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
import qualified Aoc.Day10Parser
import qualified Aoc.Day11
import qualified Aoc.Day12
import qualified Aoc.Day13
import qualified Aoc.Day14
import qualified Aoc.Day15
import qualified Aoc.Day16
import qualified Aoc.Day16Parser
import qualified Aoc.Day17
import qualified Aoc.Day18
-- import qualified Aoc.Day19
import qualified Aoc.Day20
import qualified Aoc.Day21
import qualified Aoc.Day22
import qualified Aoc.Day23
-- import qualified Aoc.Day24
import qualified Aoc.Day25
import qualified Aoc.Parser as P
import Aoc.Solution
import qualified Data.Text.IO
import qualified Expect
import qualified System.Environment
import Test (Test, describe, test)
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = do
  tests <- buildTests
  Test.run (describe "AOC 2021" tests)

buildTests :: Prelude.IO (List Test)
buildTests = do
  env <- System.Environment.getEnv "DAY"
  let day =
        if env /= ""
          then "Day" ++ Text.fromList env
          else ""
  tests <-
    Prelude.sequence
      [ mkTests day Aoc.Day01.solution "Day01",
        mkTests day Aoc.Day02.solution "Day02",
        mkTests day Aoc.Day03.solution "Day03",
        mkTests day Aoc.Day04.solution "Day04",
        mkTests day Aoc.Day05.solution "Day05",
        mkTests day Aoc.Day06.solution "Day06",
        mkTests day Aoc.Day07.solution "Day07",
        mkTests day Aoc.Day08.solution "Day08",
        mkTests day Aoc.Day09.solution "Day09",
        mkTests day Aoc.Day10.solution "Day10",
        mkTests day Aoc.Day10Parser.solution "Day10Parser",
        mkTests day Aoc.Day11.solution "Day11",
        mkTests day Aoc.Day12.solution "Day12",
        mkTests day Aoc.Day13.solution "Day13",
        mkTests day Aoc.Day14.solution "Day14",
        mkTests day Aoc.Day15.solution "Day15",
        mkTests day Aoc.Day16.solution "Day16",
        mkTests day Aoc.Day16Parser.solution "Day16Parser",
        mkTests day Aoc.Day17.solution "Day17",
        mkTests day Aoc.Day18.solution "Day18",
        -- [ mkTests day Aoc.Day19.solution "Day19"
        mkTests day Aoc.Day20.solution "Day20",
        mkTests day Aoc.Day21.solution "Day21",
        mkTests day Aoc.Day22.solution "Day22",
        mkTests day Aoc.Day23.solution "Day23",
        -- mkTests day Aoc.Day24.solution "Day24",
        mkTests day Aoc.Day25.solution "Day25"
      ]
  List.filterMap identity tests
    |> Prelude.pure

mkTests :: Text -> Solution -> Text -> Prelude.IO (Maybe Test)
mkTests day solution name
  | day == name || day == "" =
    map Just <| do
      tests <-
        Prelude.sequence
          [ mkTest solution name Part1 Example,
            mkTest solution name Part1 Real,
            mkTest solution name Part2 Example,
            mkTest solution name Part2 Real
          ]
      Prelude.pure (describe name tests)
  | Prelude.otherwise = Prelude.pure Nothing

data Run = Example | Real
  deriving (Show)

data Part = Part1 | Part2
  deriving (Show)

mkTest :: Solution -> Text -> Part -> Run -> Prelude.IO Test
mkTest Solution {parser, display, solution1, solution2} name part run = do
  let asset = case run of
        Real -> name
        Example -> name ++ "-example"
  input <-
    "test/assets/" ++ asset ++ ".txt"
      |> Text.toLower
      |> Text.toList
      |> Data.Text.IO.readFile
  let golden = Text.toLower (asset ++ "-" ++ Debug.toString part)
  input <-
    "test/assets/" ++ asset ++ ".txt"
      |> Text.toLower
      |> Text.toList
      |> Data.Text.IO.readFile
  let parsed = P.parse parser input
  let result = case part of
        Part1 -> Result.map solution1 parsed
        Part2 -> Result.map solution2 parsed
  Prelude.pure <| test (Debug.toString part ++ " " ++ Debug.toString run)
    <| \() -> do
      case result of
        Err err -> Expect.fail (Debug.toString err)
        Ok res ->
          display res
            |> Expect.equalToContentsOf ("test/golden-results/" ++ golden ++ ".hs")
