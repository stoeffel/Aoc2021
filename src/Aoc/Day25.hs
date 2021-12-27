module Aoc.Day25 (solution) where

import Aoc.Grid (Coord (..), Grid)
import qualified Aoc.Grid as Grid
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Prelude (pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = identity,
      S.visualize = Nothing
    }

data Cucumber = East | South
  deriving (Eq, Ord, Show)

parser :: P.Parser (Grid Cucumber)
parser = do
  cucumbers <-
    P.many1 cucumberParser
      |> P.lines
  cucumbers
    |> List.indexedMap (\y -> List.indexedMap (\x -> Maybe.map ((,) Coord {x, y})))
    |> List.concat
    |> List.filterMap identity
    |> Grid.fromCoords
    |> pure

cucumberParser :: P.Parser (Maybe Cucumber)
cucumberParser =
  P.keychars
    [ ('>', Just East),
      ('v', Just South),
      ('.', Nothing)
    ]

solution1 :: Grid Cucumber -> Text
solution1 cucumbers =
  cucumbers
    |> step 1
    |> Tuple.first
    |> Debug.toString

solution2 :: Grid Cucumber -> Text
solution2 _ = "Not necessary, christmas is saved!"

step :: Int -> Grid Cucumber -> (Int, Grid Cucumber)
step n grid =
  let (movedEast, gridAfterEast) = stepHerd East grid
      (movedSouth, gridAfterSouth) = stepHerd South gridAfterEast
   in if movedEast || movedSouth
        then step (n + 1) gridAfterSouth
        else (n, gridAfterSouth)

stepHerd :: Cucumber -> Grid Cucumber -> (Bool, Grid Cucumber)
stepHerd direction grid =
  Grid.foldWithKey
    ( \c@Coord {x, y} v (moved, acc) ->
        if v == direction
          then case Grid.get (nextCoord c) grid of
            Just _ -> (moved, Grid.insert c v acc)
            Nothing ->
              ( True,
                Grid.remove c acc
                  |> Grid.insert (nextCoord c) v
              )
          else (moved, Grid.insert c v acc)
    )
    (False, Grid.empty)
    grid
  where
    nextCoord c@Coord {x, y} =
      case direction of
        East -> c {x = if x == Grid.maxX grid then 0 else x + 1}
        South -> c {y = if y == Grid.maxY grid then 0 else y + 1}
