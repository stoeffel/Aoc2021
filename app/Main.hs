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
import qualified Aoc.Day16Parser
import qualified Aoc.Day17
import qualified Aoc.Day18
import qualified Aoc.Day19
import qualified Aoc.Day20
import qualified Aoc.Day21
import qualified Aoc.Day22
import qualified Aoc.Day23
import qualified Aoc.Day24
import qualified Aoc.Day25
import qualified Aoc.Grid as G
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.Text.IO
import qualified System.Environment
import qualified Prelude

main :: Prelude.IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["1", part] -> runSolution part Aoc.Day01.solution "test/assets/day01.txt"
    ["2", part] -> runSolution part Aoc.Day02.solution "test/assets/day02.txt"
    ["3", part] -> runSolution part Aoc.Day03.solution "test/assets/day03.txt"
    ["4", part] -> runSolution part Aoc.Day04.solution "test/assets/day04.txt"
    ["5", part] -> runSolution part Aoc.Day05.solution "test/assets/day05.txt"
    ["6", part] -> runSolution part Aoc.Day06.solution "test/assets/day06.txt"
    ["7", part] -> runSolution part Aoc.Day07.solution "test/assets/day07.txt"
    ["8", part] -> runSolution part Aoc.Day08.solution "test/assets/day08.txt"
    ["9", part] -> runSolution part Aoc.Day09.solution "test/assets/day09.txt"
    ["10", part] -> runSolution part Aoc.Day10.solution "test/assets/day10.txt"
    ["11", part] -> runSolution part Aoc.Day11.solution "test/assets/day11.txt"
    ["12", part] -> runSolution part Aoc.Day12.solution "test/assets/day12.txt"
    ["13", part] -> runSolution part Aoc.Day13.solution "test/assets/day13.txt"
    ["14", part] -> runSolution part Aoc.Day14.solution "test/assets/day14.txt"
    ["15", part] -> runSolution part Aoc.Day15.solution "test/assets/day15.txt"
    ["15", part, "example"] -> runSolution part Aoc.Day15.solution "test/assets/day15-example.txt"
    ["16", part, "example"] -> runSolution part Aoc.Day16.solution "test/assets/day16-example.txt"
    ["16", part, "parser"] -> runSolution part Aoc.Day16Parser.solution "test/assets/day16.txt"
    ["16", part] -> runSolution part Aoc.Day16.solution "test/assets/day16.txt"
    ["17", part] -> runSolution part Aoc.Day17.solution "test/assets/day17.txt"
    ["18", part] -> runSolution part Aoc.Day18.solution "test/assets/day18.txt"
    ["19", part] -> runSolution part Aoc.Day19.solution "test/assets/day19.txt"
    ["19", part, "example"] -> runSolution part Aoc.Day19.solution "test/assets/day19-example.txt"
    ["20", part] -> runSolution part Aoc.Day20.solution "test/assets/day20.txt"
    ["21", part] -> runSolution part Aoc.Day21.solution "test/assets/day21.txt"
    ["22", part] -> runSolution part Aoc.Day22.solution "test/assets/day22.txt"
    ["23", part] -> runSolution part Aoc.Day23.solution "test/assets/day23.txt"
    ["24", part] -> runSolution part Aoc.Day24.solution "test/assets/day24.txt"
    ["25", part] -> runSolution part Aoc.Day25.solution "test/assets/day25.txt"
    _ -> Debug.todo "aoc {day} {part}"

runSolution :: Prelude.String -> S.Solution -> Prelude.String -> Prelude.IO ()
runSolution part S.Solution {S.parser, S.solution1, S.solution2, S.display, S.visualize} path = do
  input <- Data.Text.IO.readFile path
  let parsed = P.parse parser input
  case parsed of
    Err err -> Debug.todo err
    Ok p -> do
      case part of
        "1" -> Prelude.putStrLn (Text.toList (display (solution1 p)))
        "2" -> Prelude.putStrLn (Text.toList (display (solution2 p)))
        "viz" -> case visualize of
          Just f -> f p
          Nothing -> Debug.todo "visualize not implemented"
        _ -> Debug.todo "Part not found"
