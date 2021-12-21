module Aoc.Day21 (solution) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Dict (Dict)
import qualified Dict
import Set (Set)
import qualified Set

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser Text
parser = map Text.fromList (P.many P.anyChar)

solution1 :: Text -> Int
solution1 _ = 1067724 -- in ../prolog/day21_part1.pl

solution2 :: Text -> Int
solution2 _ =
  game
    One
    ( Counter.empty,
      Counter.fromList
        [State {one = PlayerState 5 0, two = PlayerState 8 0}]
    )
    |> Counter.max
    |> Maybe.map Tuple.second
    |> Maybe.withDefault 0

data Player = One | Two
  deriving (Eq, Ord, Show)

data State = State
  { one :: PlayerState,
    two :: PlayerState
  }
  deriving (Eq, Ord, Show)

data PlayerState = PlayerState
  {position :: Int, score :: Int}
  deriving (Eq, Ord, Show)

type Wins = Counter Player

game :: Player -> (Wins, Counter State) -> Wins
game player (wins, states) =
  if Counter.isEmpty states
    then wins
    else
      Counter.foldl
        ( \s c acc ->
            List.foldl (\s -> Counter.add s c) acc (turn player s)
        )
        Counter.empty
        states
        |> updateWins player wins
        |> game (nextPlayer player)

updateWins :: Player -> Wins -> Counter State -> (Wins, Counter State)
updateWins player wins states =
  Counter.foldl
    ( \state c (w, acc) ->
        case getState player state of
          playerState ->
            if score playerState >= 21
              then (Counter.add player c w, acc)
              else (w, Counter.add state c acc)
    )
    (wins, Counter.empty)
    states

nextPlayer :: Player -> Player
nextPlayer = \case
  One -> Two
  Two -> One

turn :: Player -> State -> List State
turn player state =
  getState player state
    |> rollQuantumDice
    |> List.map (updateState player state)

getState :: Player -> State -> PlayerState
getState = \case
  One -> one
  Two -> two

updateState :: Player -> State -> PlayerState -> State
updateState player state newState =
  case player of
    One -> state {one = newState}
    Two -> state {two = newState}

rollQuantumDice :: PlayerState -> List PlayerState
rollQuantumDice state@PlayerState {position, score} =
  quantumDice
    |> List.map
      ( \(a, b, c) ->
          let absPos = position + a + b + c
              newPos =
                if modBy 10 absPos == 0
                  then 10
                  else modBy 10 absPos
           in PlayerState
                { position = newPos,
                  score = score + newPos
                }
      )

quantumDice :: List (Int, Int, Int)
quantumDice =
  [ (1, 1, 1),
    (1, 1, 2),
    (1, 1, 3),
    (1, 2, 1),
    (1, 2, 2),
    (1, 2, 3),
    (1, 3, 1),
    (1, 3, 2),
    (1, 3, 3),
    (2, 1, 1),
    (2, 1, 2),
    (2, 1, 3),
    (2, 2, 1),
    (2, 2, 2),
    (2, 2, 3),
    (2, 3, 1),
    (2, 3, 2),
    (2, 3, 3),
    (3, 1, 1),
    (3, 1, 2),
    (3, 1, 3),
    (3, 2, 1),
    (3, 2, 2),
    (3, 2, 3),
    (3, 3, 1),
    (3, 3, 2),
    (3, 3, 3)
  ]
