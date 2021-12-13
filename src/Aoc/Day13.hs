{-# LANGUAGE TupleSections #-}

module Aoc.Day13 (solution, Dot) where

import Aoc.Grid (Coord (..), Grid)
import qualified Aoc.Grid as Grid
import Aoc.Parser ((<*))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Control.Concurrent
import Control.Monad (foldM_)
import System.Console.ANSI (clearScreen)
import Prelude (IO, pure, putStrLn, uncurry)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = identity,
      S.visualize = Just visualize
    }

data Dot = Dot deriving (Eq, Show)

data Fold = Fold
  { orientation :: Orientation,
    value :: Int
  }
  deriving (Eq, Show)

data Orientation = Horizontal | Vertical deriving (Eq, Show)

parser :: P.Parser (Grid Dot, Fold, List Fold)
parser = do
  dots <- P.lines (map (,Dot) coordParser) <* P.endOfLine
  firstFold <- foldParser <* P.endOfLine
  folds <- P.lines foldParser
  pure (Grid.fromCoords dots, firstFold, folds)

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

solution1 :: (Grid Dot, Fold, List Fold) -> Text
solution1 (grid, fold, _) =
  foldGrid fold grid
    |> Grid.length
    |> Debug.toString

solution2 :: (Grid Dot, Fold, List Fold) -> Text
solution2 (grid, fold, folds) =
  List.foldl foldGrid grid (fold : folds)
    |> Grid.toText dotToText

foldGrid :: Fold -> Grid a -> Grid a
foldGrid Fold {orientation, value} grid =
  Grid.mapKeys
    ( \coord ->
        if get coord >= value
          then set coord (modBy max (max - get coord))
          else coord
    )
    grid
  where
    (get, max, set) = case orientation of
      Vertical -> (x, Grid.maxX grid, \c x -> c {x})
      Horizontal -> (y, Grid.maxY grid, \c y -> c {y})

dotToText :: Maybe Dot -> Text
dotToText = \case
  Nothing -> " "
  Just Dot -> "█"

visualize :: (Grid Dot, Fold, List Fold) -> IO ()
visualize (grid, fold, folds) =
  foldM_
    ( \acc fold -> do
        clearScreen
        let next = foldGrid fold acc
        Grid.toText dotToText next
          |> Text.toList
          |> putStrLn
        Control.Concurrent.threadDelay 100000
        pure next
    )
    grid
    (fold : folds)
