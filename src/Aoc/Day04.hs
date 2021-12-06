{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Aoc.Day04 (solution) where

import Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.List
import qualified Prelude

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Game = Game
  { boards :: List (Board Cell),
    draws :: List Int
  }
  deriving (Show)

newtype Board a = Board {rows :: List (List a)}
  deriving (Show, Eq, Prelude.Functor, Prelude.Foldable)

mapBoard :: (a -> b) -> Board a -> Board b
mapBoard = Prelude.fmap

data Cell = Cell {marked :: Marked, value :: Int}
  deriving (Show, Eq)

data Marked
  = Marked
  | Unmarked
  deriving (Show, Eq)

parser :: P.Parser Game
parser = do
  draws <- P.csv P.decimal
  _ <- P.endOfLine
  boards <- P.lines boardParser
  Prelude.pure (Game {boards, draws})

boardParser :: P.Parser (Board Cell)
boardParser =
  P.count 5 (P.endOfLine *> P.count 5 cellParser)
    |> map Board

cellParser :: P.Parser Cell
cellParser =
  P.many (P.char ' ') *> P.decimal
    |> map (Cell Unmarked)

solution1 :: Game -> Maybe Int
solution1 Game {draws, boards} =
  play boards draws
    |> Maybe.map Tuple.first
    |> Maybe.map calcResult

solution2 :: Game -> Maybe Int
solution2 Game {draws, boards} =
  playUntilLast Nothing draws boards
    |> Maybe.map calcResult

data Winner = Winner
  { lastDraw :: Int,
    board :: Board Cell
  }

calcResult :: Winner -> Int
calcResult Winner {lastDraw, board} =
  lastDraw * sumUnmarked
  where
    sumUnmarked =
      Prelude.foldl
        ( \acc Cell {value, marked} -> case marked of
            Unmarked -> acc + value
            Marked -> acc
        )
        0
        board

playUntilLast :: Maybe Winner -> List Int -> List (Board Cell) -> Maybe Winner
playUntilLast lastWinner draws boards =
  play boards draws
    |> ( \case
           Just (winner, newBoards) ->
             List.filter (/= board winner) newBoards
               |> playUntilLast (Just winner) draws
           Nothing -> lastWinner
       )

play :: List (Board Cell) -> List Int -> Maybe (Winner, List (Board Cell))
play boards [] = Nothing
play boards (lastDraw : draws) =
  case hasWinner newBoards of
    Just board -> Just (Winner {lastDraw, board}, newBoards)
    Nothing -> play newBoards draws
  where
    newBoards = List.map (mapBoard (playDraw lastDraw)) boards

    hasWinner :: List (Board Cell) -> Maybe (Board Cell)
    hasWinner =
      Data.List.find
        ( \Board {rows} ->
            List.any
              (List.all (\Cell {marked} -> marked == Marked))
              (rows ++ Data.List.transpose rows)
        )

    playDraw :: Int -> Cell -> Cell
    playDraw draw cell =
      if value cell == draw
        then cell {marked = Marked}
        else cell
