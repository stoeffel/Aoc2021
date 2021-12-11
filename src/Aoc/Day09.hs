module Aoc.Day09 (solution) where

import Aoc.Grid (Coord, Grid)
import qualified Aoc.Grid as Grid
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Set (Set)
import qualified Set
import Prelude (flip, otherwise, pure)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

parser :: P.Parser (Grid Int)
parser = do
  points <- P.lines (P.many1 P.digitInt)
  points
    |> Grid.fromLists
    |> pure

solution1 :: Grid Int -> Int
solution1 input =
  input
    |> Grid.foldWithKey (findLowPoints input) []
    |> List.foldl (\(_, x) acc -> x + 1 + acc) 0

solution2 :: Grid Int -> Int
solution2 input =
  input
    |> Grid.foldWithKey (findLowPoints input) []
    |> List.map (Tuple.first >> findBasins input Set.empty >> Set.size)
    |> List.sortWith desc
    |> List.take 3
    |> List.foldl (*) 1

findLowPoints :: Grid Int -> Coord -> Int -> List (Coord, Int) -> List (Coord, Int)
findLowPoints all coord v acc =
  if isLowPoint v
    then (coord, v) : acc
    else acc
  where
    isLowPoint :: Int -> Bool
    isLowPoint cell =
      Grid.neightbours coord
        |> List.filterMap (flip Grid.get all)
        |> List.all (> cell)

findBasins :: Grid Int -> Set Coord -> Coord -> Set Coord
findBasins all acc coord
  | Set.member coord acc = acc
  | otherwise = case Grid.get coord all of
    Nothing -> acc
    Just 9 -> acc
    Just _ ->
      List.foldl
        (flip (findBasins all))
        (Set.insert coord acc)
        (Grid.neightbours coord)

desc :: Int -> Int -> Ordering
desc a b = case compare a b of
  LT -> GT
  EQ -> EQ
  GT -> LT
