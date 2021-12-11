module Aoc.Day11 (solution) where

import Aoc.Grid (Grid)
import qualified Aoc.Grid as Grid
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Prelude (flip, pure)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Octopus = Octopus {energy :: Int, flashed :: Int, justFlashed :: Bool}
  deriving (Show, Eq)

parser :: P.Parser (Grid Octopus)
parser = do
  points <- P.lines (P.many1 P.digitInt)
  points
    |> Grid.fromLists
    |> Grid.map
      ( \energy ->
          Octopus {energy, flashed = 0, justFlashed = False}
      )
    |> pure

solution1 :: Grid Octopus -> Int
solution1 octopuses =
  octopuses
    |> step 100
    |> Grid.map flashed
    |> Grid.sum

solution2 :: Grid Octopus -> Int
solution2 octopuses = synchronized 0 octopuses

step :: Int -> Grid Octopus -> Grid Octopus
step 0 octopuses = octopuses
step n octopuses =
  octopuses
    |> Grid.map incEnergy
    |> flashOctopuses
    |> Grid.map goDark
    |> step (n - 1)

synchronized :: Int -> Grid Octopus -> Int
synchronized x octopuses =
  let newOctopuses =
        octopuses
          |> Grid.map incEnergy
          |> flashOctopuses
   in if Grid.all justFlashed newOctopuses
        then x + 1
        else
          newOctopuses
            |> Grid.map goDark
            |> synchronized (x + 1)

flashOctopuses :: Grid Octopus -> Grid Octopus
flashOctopuses octopuses =
  if Grid.all (\Octopus {energy} -> energy <= 9) octopuses
    then octopuses
    else
      Grid.foldWithKey
        ( \coord Octopus {energy, justFlashed} acc ->
            if energy <= 9
              then acc
              else
                List.foldl
                  (flip Grid.update incEnergy)
                  acc
                  (Grid.surrounding coord)
                  |> Grid.update coord flash
        )
        octopuses
        octopuses
        |> flashOctopuses

incEnergy :: Octopus -> Octopus
incEnergy f = f {energy = energy f + 1}

flash :: Octopus -> Octopus
flash f =
  f
    { energy = 0,
      flashed = flashed f + 1,
      justFlashed = True
    }

goDark :: Octopus -> Octopus
goDark f =
  if justFlashed f
    then f {justFlashed = False, energy = 0}
    else f
