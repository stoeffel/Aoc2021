{-# LANGUAGE TupleSections #-}

module Aoc.Day13 (solution, Dot) where

import Aoc.Grid (Coord (..), Grid)
import qualified Aoc.Grid as Grid
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Prelude (pure, uncurry)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2, S.display = identity}

data Dot = Dot deriving (Eq, Show)

data Fold = Fold
  { orientation :: Orientation,
    value :: Int
  }
  deriving (Eq, Show)

data Orientation = Horizontal | Vertical deriving (Eq, Show)

parser :: P.Parser (Grid Dot, List Fold)
parser = do
  dots <- P.lines (map (,Dot) coordParser)
  _ <- P.endOfLine
  folds <- P.lines foldParser
  pure (Grid.fromCoords dots, folds)

coordParser :: P.Parser Coord
coordParser = do
  x <- P.decimal
  _ <- P.char ','
  y <- P.decimal
  pure Coord {x, y}

foldParser :: P.Parser Fold
foldParser = do
  _ <- P.string "fold along "
  orientation <-
    P.keywords
      [ ("x=", Vertical),
        ("y=", Horizontal)
      ]
  value <- P.decimal
  pure Fold {orientation, value}

solution1 :: (Grid Dot, List Fold) -> Text
solution1 (grid, folds) =
  case folds of
    [] -> ""
    fold : _ ->
      foldGrid fold grid
        |> Grid.values
        |> List.length
        |> Text.fromInt

solution2 :: (Grid Dot, List Fold) -> Text
solution2 (grid, folds) =
  List.foldl foldGrid grid folds
    |> Grid.toLists
    |> List.map (Text.join "" << List.map dotToText)
    |> Text.join "\n"

dotToText :: Maybe Dot -> Text
dotToText = \case
  Nothing -> "."
  Just Dot -> "#"

foldGrid :: Fold -> Grid a -> Grid a
foldGrid Fold {orientation, value} grid =
  splitGrid value orientation grid
    |> Tuple.mapSecond (flipGrid orientation)
    |> uncurry Grid.union

splitGrid :: Int -> Orientation -> Grid a -> (Grid a, Grid a)
splitGrid target orientation g =
  let fromCoord Coord {x, y} =
        case orientation of
          Vertical -> x
          Horizontal -> y
   in Grid.partition (\c _ -> fromCoord c <= target) g
        |> Tuple.mapFirst
          (Grid.filter (\c _ -> fromCoord c /= target))

flipGrid :: Orientation -> Grid a -> Grid a
flipGrid orientation g =
  Grid.mapKeys
    ( \c -> case orientation of
        Horizontal -> c {y = flip (Grid.maxY g) (y c)}
        Vertical -> c {x = flip (Grid.maxX g) (x c)}
    )
    g

flip :: Int -> Int -> Int
flip maxX x =
  if x == 0
    then maxX
    else modBy maxX (maxX - x)
