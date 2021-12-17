module Aoc.Day15 (solution) where

import Aoc.Grid (Coord (..), Grid)
import qualified Aoc.Grid as Grid
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.HashPSQ as Q
import Dict (Dict)
import qualified Dict as Dict
import Prelude (flip, pure, uncurry)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser (Grid Int)
parser = do
  points <- P.lines (P.many1 P.digitInt)
  points
    |> Grid.fromLists
    |> pure

solution1 :: Grid Int -> Maybe Int
solution1 grid = findCheapestPath grid

solution2 :: Grid Int -> Maybe Int
solution2 grid =
  expandGrid 5 grid
    |> findCheapestPath

expandGrid :: Int -> Grid Int -> Grid Int
expandGrid a grid =
  let maxX = Grid.maxX grid + 1
      maxY = Grid.maxY grid + 1
   in grid
        |> List.repeat a
        |> List.repeat a
        |> List.indexedMap (\y -> List.indexedMap (transposeGrid maxX maxY a y))
        |> List.concat
        |> List.foldl Grid.union Grid.empty

transposeGrid :: Int -> Int -> Int -> Int -> Int -> Grid Int -> Grid Int
transposeGrid maxX maxY n j i grid =
  grid
    |> Grid.mapKeys
      ( \(Coord x y) ->
          Coord
            { x = x + (i * maxX),
              y = y + (j * maxY)
            }
      )
    |> Grid.map (\v -> wrap (v + (i + j) * 1))

wrap :: Int -> Int
wrap x =
  let remainder = modBy 9 x
   in if remainder == 0 then 9 else remainder

type Frontier = Q.HashPSQ Coord Int ()

type CostSoFar = Dict Coord Int

findCheapestPath :: Grid Int -> Maybe Int
findCheapestPath grid =
  cheapestPath
    (Coord {x = Grid.maxX grid, y = Grid.maxY grid})
    grid
    (Q.singleton start 0 ())
    (Dict.singleton start 0)

start :: Coord
start = Coord {x = 0, y = 0}

cheapestPath :: Coord -> Grid Int -> Frontier -> CostSoFar -> Maybe Int
cheapestPath target grid frontier costSoFar =
  case Q.minView frontier of
    Nothing -> Nothing
    Just (coord, _, _, _) | coord == target -> Dict.get target costSoFar
    Just (coord, _, _, newFrontier) ->
      Grid.neighbors coord
        |> List.filterMap (\n -> Maybe.map ((,) n) (Grid.get n grid))
        |> List.foldl (updateNeighbor target coord) (newFrontier, costSoFar)
        |> uncurry (cheapestPath target grid)

updateNeighbor :: Coord -> Coord -> (Coord, Int) -> (Frontier, CostSoFar) -> (Frontier, CostSoFar)
updateNeighbor target current (neighbor, neighborCost) (frontier, costSoFar) =
  case Dict.get current costSoFar of
    Nothing -> (frontier, costSoFar)
    Just currentCost ->
      let newCost = currentCost + neighborCost
          priority = newCost + heuristic target neighbor
       in case Dict.get neighbor costSoFar of
            Just c ->
              if newCost < c
                then
                  ( Q.insert neighbor priority () frontier,
                    Dict.insert neighbor newCost costSoFar
                  )
                else (frontier, costSoFar)
            Nothing ->
              ( Q.insert neighbor priority () frontier,
                Dict.insert neighbor newCost costSoFar
              )

heuristic :: Coord -> Coord -> Int
heuristic a b =
  abs (x a - x b) + abs (y a - y b)
