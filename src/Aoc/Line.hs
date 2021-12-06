module Aoc.Line
  ( Line,
    parser,
    intersections,
    orientation,
    Orientation (..),
  )
where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import Prelude (flip, otherwise, pure)

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
orientation Line {start, end}
  | x start == x end = Vertical
  | y start == y end = Horizontal
  | otherwise = Diagonal

intersections :: List Line -> Counter Point
intersections lines =
  lines
    |> List.concatMap lineToPoints
    |> Counter.count
  where
    range :: Int -> Int -> List Int
    range x y = if x < y then [x .. y] else [y .. x]

    lineToPoints :: Line -> List Point
    lineToPoints line@Line {start, end} =
      case orientation line of
        Horizontal ->
          List.map (flip Point (y start)) (range (x start) (x end))
        Vertical ->
          List.map (Point (x start)) (range (y start) (y end))
        Diagonal ->
          let slope' = slope line
           in List.map
                ( \onX ->
                    Point onX (y start + slope' * (onX - x start))
                )
                (range (x start) (x end))

slope :: Line -> Int
slope Line {start, end} = (y end - y start) // (x end - x start)
