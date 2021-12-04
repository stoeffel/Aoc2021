{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Aoc.Day04 (Day04 (..)) where

import Aoc.Helpers (Solution (..))
import Aoc.Parser as P
import Control.Applicative ((<|>))
import qualified Data.List
import qualified List
import qualified Prelude

data Day04 = Day04

instance Solution Day04 where
  solution1 _ = part1 >> Debug.toString
  solution2 _ = part2 >> Debug.toString

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

gameParser :: P.Parser Game
gameParser = do
  draws <- P.sepBy1 P.decimal (P.char ',')
  _ <- P.count 2 P.endOfLine
  boards <- P.sepBy1 boardParser P.endOfLine
  Prelude.pure (Game {boards, draws})

boardParser :: P.Parser (Board Cell)
boardParser =
  P.count 5 (P.count 5 cellParser <* P.endOfLine)
    |> map Board

cellParser :: P.Parser Cell
cellParser =
  P.many (P.char ' ') *> P.decimal
    |> map (Cell Unmarked)

part1 :: Text -> Maybe Int
part1 input =
  let Game {draws, boards} = P.unsafeParse gameParser input
   in play boards draws
        |> Maybe.map Tuple.first
        |> Maybe.map calcResult

part2 :: Text -> Maybe Int
part2 input =
  let Game {draws, boards} = P.unsafeParse gameParser input
   in playUntilLast Nothing draws boards
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
