module Aoc.Day10 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Aoc.Stack (Stack)
import qualified Aoc.Stack as Stack
import Prelude (otherwise)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Bracket = Angle | Curly | Round | Square
  deriving (Eq, Show)

data OpenClose a = Open a | Close a
  deriving (Eq, Show)

parser :: P.Parser (List (List (OpenClose Bracket)))
parser = P.lines (P.many1 bracketParser)

bracketParser :: P.Parser (OpenClose Bracket)
bracketParser =
  P.keywords
    [ ("(", Open Round),
      ("{", Open Curly),
      ("[", Open Square),
      ("<", Open Angle),
      (")", Close Round),
      ("}", Close Curly),
      ("]", Close Square),
      (">", Close Angle)
    ]

solution1 :: List (List (OpenClose Bracket)) -> Maybe Int
solution1 input =
  List.map (syntaxCheck Stack.empty) input
    |> List.foldl (\result acc -> acc + syntaxErrorScore result) 0
    |> Just

solution2 :: List (List (OpenClose Bracket)) -> Maybe Int
solution2 input =
  List.map (syntaxCheck Stack.empty) input
    |> List.filterMap incompleteLines
    |> List.map (Stack.foldl (\acc x -> acc * 5 + autocompleteScore x) 0)
    |> List.sort
    |> (\xs -> List.drop (List.length xs // 2) xs)
    |> List.head

data SyntaxCheckResult
  = Successful
  | Corrupted Bracket
  | Incomplete (Stack Bracket)
  deriving (Show)

syntaxCheck :: Stack Bracket -> List (OpenClose Bracket) -> SyntaxCheckResult
syntaxCheck stack []
  | Stack.isEmpty stack = Successful
  | otherwise = Incomplete stack
syntaxCheck stack (Open x : xs) =
  syntaxCheck (Stack.push x stack) xs
syntaxCheck stack (Close x : xs)
  | Stack.isEmpty stack = Corrupted x
  | otherwise =
    case Stack.pop stack of
      (Just expected, rest) ->
        if x == expected
          then syntaxCheck rest xs
          else Corrupted x
      (Nothing, _) -> Corrupted x

incompleteLines :: SyntaxCheckResult -> Maybe (Stack Bracket)
incompleteLines = \case
  Incomplete stack -> Just stack
  otherwise -> Nothing

syntaxErrorScore :: SyntaxCheckResult -> Int
syntaxErrorScore = \case
  Successful -> 0
  Incomplete _ -> 0
  Corrupted Round -> 3
  Corrupted Square -> 57
  Corrupted Curly -> 1197
  Corrupted Angle -> 25137

autocompleteScore :: Bracket -> Int
autocompleteScore = \case
  Round -> 1
  Square -> 2
  Curly -> 3
  Angle -> 4
