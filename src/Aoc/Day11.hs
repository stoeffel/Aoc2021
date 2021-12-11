module Aoc.Day11 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Dict (Dict)
import qualified Dict
import Prelude (pure)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Coord = Coord {x :: Int, y :: Int}
  deriving (Show, Eq, Ord)

data Flash = Flash {energy :: Int, flashed :: Int, justFlashed :: Bool}
  deriving (Show, Eq)

parser :: P.Parser (Dict Coord Flash)
parser = do
  points <- P.lines (P.many1 P.digitInt)
  points
    |> List.indexedMap
      ( \x ->
          List.indexedMap
            ( \y energy ->
                ( Coord {x, y},
                  Flash {energy, flashed = 0, justFlashed = False}
                )
            )
      )
    |> List.concat
    |> Dict.fromList
    |> pure

solution1 :: Dict Coord Flash -> Int
solution1 octopuses =
  octopuses
    |> step 100
    |> Dict.foldl (\_ Flash {flashed} acc -> flashed + acc) 0

solution2 :: Dict Coord Flash -> Int
solution2 octopuses = synchronized 0 octopuses

step :: Int -> Dict Coord Flash -> Dict Coord Flash
step 0 octopuses = octopuses
step n octopuses =
  octopuses
    |> Dict.map (\_ -> incEnergy)
    |> flashOctopuses
    |> Dict.map (\_ -> goDark)
    |> step (n - 1)

synchronized :: Int -> Dict Coord Flash -> Int
synchronized x octopuses =
  let newOctopuses =
        octopuses
          |> Dict.map (\_ -> incEnergy)
          |> flashOctopuses
   in if List.all justFlashed (Dict.values newOctopuses)
        then x + 1
        else
          newOctopuses
            |> Dict.map (\_ -> goDark)
            |> synchronized (x + 1)

flashOctopuses :: Dict Coord Flash -> Dict Coord Flash
flashOctopuses octopuses =
  if List.all (\Flash {energy} -> energy <= 9) (Dict.values octopuses)
    then octopuses
    else
      Dict.foldl
        ( \coord Flash {energy, justFlashed} acc ->
            if energy <= 9
              then acc
              else
                List.foldl
                  (\x -> Dict.update x (Maybe.map incEnergy))
                  acc
                  (neightbours coord)
                  |> Dict.update coord (Maybe.map flash)
        )
        octopuses
        octopuses
        |> flashOctopuses

incEnergy :: Flash -> Flash
incEnergy f = f {energy = energy f + 1}

flash :: Flash -> Flash
flash f =
  f
    { energy = 0,
      flashed = flashed f + 1,
      justFlashed = True
    }

goDark :: Flash -> Flash
goDark f =
  if justFlashed f
    then f {justFlashed = False, energy = 0}
    else f

neightbours :: Coord -> List Coord
neightbours coord@Coord {x, y} =
  [ coord {y = y - 1},
    coord {x = x + 1},
    coord {y = y + 1},
    coord {x = x - 1},
    coord {x = x - 1, y = y - 1},
    coord {x = x + 1, y = y - 1},
    coord {x = x - 1, y = y + 1},
    coord {x = x + 1, y = y + 1}
  ]
