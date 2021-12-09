module Aoc.Day09 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Dict (Dict)
import qualified Dict
import Set (Set)
import qualified Set
import Prelude (flip, otherwise, pure)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Coord = Coord {x :: Int, y :: Int}
  deriving (Eq, Ord)

parser :: P.Parser (Dict Coord Int)
parser = do
  points <- P.lines (P.many1 P.digitInt)
  points
    |> List.indexedMap (\x -> List.indexedMap (\y v -> (Coord {x, y}, v)))
    |> List.concat
    |> Dict.fromList
    |> pure

solution1 :: Dict Coord Int -> Int
solution1 input =
  input
    |> Dict.foldl (findLowPoints input) []
    |> List.foldl (\(_, x) acc -> x + 1 + acc) 0

solution2 :: Dict Coord Int -> Int
solution2 input =
  input
    |> Dict.foldl (findLowPoints input) []
    |> List.map (Tuple.first >> findBasins input Set.empty >> Set.size)
    |> List.sortWith desc
    |> List.take 3
    |> List.foldl (*) 1

findLowPoints :: Dict Coord Int -> Coord -> Int -> List (Coord, Int) -> List (Coord, Int)
findLowPoints all coord v acc =
  case Dict.get coord all of
    Just cell | isLowPoint cell -> (coord, cell) : acc
    _ -> acc
  where
    isLowPoint :: Int -> Bool
    isLowPoint cell =
      neightbours coord
        |> List.filterMap (flip Dict.get all)
        |> List.all (> cell)

findBasins :: Dict Coord Int -> Set Coord -> Coord -> Set Coord
findBasins all acc coord
  | Set.member coord acc = acc
  | otherwise = case Dict.get coord all of
    Nothing -> acc
    Just 9 -> acc
    Just _ ->
      List.foldl
        (flip (findBasins all))
        (Set.insert coord acc)
        (neightbours coord)

neightbours :: Coord -> List Coord
neightbours coord@Coord {x, y} =
  [ coord {y = y - 1},
    coord {x = x + 1},
    coord {y = y + 1},
    coord {x = x - 1}
  ]

desc :: Int -> Int -> Ordering
desc a b = case compare a b of
  LT -> GT
  EQ -> EQ
  GT -> LT
