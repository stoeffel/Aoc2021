module Aoc.Day15 (solution) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import Aoc.Grid (Coord (..), Grid)
import qualified Aoc.Grid as Grid
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.HashPSQ as Q
import qualified Data.List
import Set (Set)
import qualified Set as Set
import Prelude (flip, pure)

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

solution1 :: Grid Int -> Int
solution1 grid = findCheapestPath grid

solution2 :: Grid Int -> Int
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

findCheapestPath :: Grid Int -> Int
findCheapestPath grid =
  cheapestPath
    (Coord {x = Grid.maxX grid, y = Grid.maxY grid})
    grid
    Set.empty
    (Q.singleton (Coord 0 0) 0 ())

cheapestPath :: Coord -> Grid Int -> Set Coord -> Q.HashPSQ Coord Int () -> Int
cheapestPath target grid visited costs =
  case Q.minView costs of
    Nothing -> 0
    Just (coord, cost, (), newCosts) ->
      if coord == target
        then cost
        else
          let newVisited = Set.insert coord visited
           in updateNeighbors grid newVisited (cost, coord) newCosts
                |> cheapestPath target grid newVisited

updateNeighbors ::
  Grid Int ->
  Set Coord ->
  (Int, Coord) ->
  Q.HashPSQ Coord Int () ->
  Q.HashPSQ Coord Int ()
updateNeighbors grid visited (currentCost, current) costs =
  Grid.neighbors current
    |> List.filter (not << flip Set.member visited)
    |> List.filterMap
      ( \n ->
          Grid.get n grid
            |> Maybe.map ((,) n)
      )
    |> List.foldl
      ( \(neighbor, neighborCost) costs ->
          let newCost = currentCost + neighborCost
           in case Q.lookup neighbor costs of
                Nothing -> Q.insert neighbor newCost () costs
                Just (c, _) ->
                  if c < newCost
                    then costs
                    else Q.insert neighbor newCost () costs
      )
      costs
