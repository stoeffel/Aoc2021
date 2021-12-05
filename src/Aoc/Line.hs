module Aoc.Line
  ( Line,
    parser,
    intersections,
    orientation,
    Orientation (..),
  )
where

import qualified Aoc.Parser as P
import Dict (Dict)
import qualified Dict
import qualified List
import Prelude (flip, pure)

data Line = Line {start :: Point, end :: Point}
  deriving (Show, Eq)

data Point = Point {x :: Int, y :: Int}
  deriving (Show, Eq, Ord)

data Orientation = Horizontal | Vertical | Diagonal
  deriving (Show, Eq)

parser :: P.Parser Line
parser = do
  start <- pointParser
  _ <- P.many1 P.space
  _ <- P.string "->"
  _ <- P.many1 P.space
  end <- pointParser
  pure Line {start, end}
  where
    pointParser :: P.Parser Point
    pointParser = do
      x <- P.decimal
      _ <- P.char ','
      y <- P.decimal
      pure Point {x, y}

orientation :: Line -> Orientation
orientation Line {start, end} =
  if x start == x end
    then Vertical
    else
      if y start == y end
        then Horizontal
        else Diagonal

intersections :: List Line -> Dict Point Int
intersections lines =
  lines
    |> List.concatMap lineToPoints
    |> List.foldl
      ( flip
          Dict.update
          ( \case
              Just count -> Just (count + 1)
              Nothing -> Just 1
          )
      )
      Dict.empty
  where
    lineToPoints :: Line -> List Point
    lineToPoints line@Line {start, end} =
      case orientation line of
        Horizontal ->
          List.map
            (flip Point (y start))
            (range (x start) (x end))
        Vertical ->
          List.map
            (Point (x start))
            (range (y start) (y end))
        Diagonal ->
          let slope' = slope line
           in List.map
                ( \onX ->
                    Point onX (y start + slope' * (onX - x start))
                )
                (range (x start) (x end))

    range :: Int -> Int -> [Int]
    range x y = if x < y then [x .. y] else [y .. x]

    slope :: Line -> Int
    slope Line {start, end} = (y end - y start) // (x end - x start)
