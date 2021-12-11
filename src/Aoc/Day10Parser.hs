module Aoc.Day10Parser (solution) where

import Aoc.Parser ((<*))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative (optional)
import qualified Data.Foldable as F
import Data.Traversable (traverse)
import Data.Tree (Tree (Node))
import Prelude (Foldable, Functor, Traversable, otherwise, pure)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Bracket = Angle | Curly | Round | Square
  deriving (Eq, Show)

data Chunk
  = Closed
  | Corrupted Bracket
  | Incomplete Bracket
  deriving (Eq, Show)

parser :: P.Parser (List (Tree Chunk))
parser = P.lines treeParser

treeParser :: P.Parser (Tree Chunk)
treeParser = do
  open <- openBracketParser
  sub <- P.many treeParser
  close <- optional closeBracketParser
  pure
    ( Node
        ( case close of
            Nothing -> Incomplete open
            Just c
              | c == open -> Closed
              | otherwise -> Corrupted c
        )
        sub
    )

openBracketParser :: P.Parser Bracket
openBracketParser =
  P.keywords
    [ ("(", Round),
      ("{", Curly),
      ("[", Square),
      ("<", Angle)
    ]

closeBracketParser :: P.Parser Bracket
closeBracketParser =
  P.keywords
    [ (")", Round),
      ("}", Curly),
      ("]", Square),
      (">", Angle)
    ]

solution1 :: List (Tree Chunk) -> Maybe Int
solution1 forest =
  List.concatMap F.toList forest
    |> List.filterMap corrupted
    |> List.map syntaxErrorScore
    |> List.sum
    |> Just

solution2 :: List (Tree Chunk) -> Maybe Int
solution2 forest =
  List.map F.toList forest
    |> List.filterMap incompleteLines
    |> List.map (List.foldr (\x acc -> acc * 5 + autocompleteScore x) 0)
    |> List.sort
    |> (\xs -> List.drop (List.length xs // 2) xs)
    |> List.head

corrupted :: Chunk -> Maybe Bracket
corrupted = \case
  Corrupted b -> Just b
  _ -> Nothing

incompleteLines :: List Chunk -> Maybe (List Bracket)
incompleteLines xs =
  if List.any (corrupted >> (/= Nothing)) xs
    then Nothing
    else case List.filterMap incomplete xs of
      [] -> Nothing
      incompleteXs -> Just incompleteXs

incomplete :: Chunk -> Maybe Bracket
incomplete = \case
  Incomplete x -> Just x
  Corrupted _ -> Nothing
  Closed -> Nothing

syntaxErrorScore :: Bracket -> Int
syntaxErrorScore = \case
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137

autocompleteScore :: Bracket -> Int
autocompleteScore = \case
  Round -> 1
  Square -> 2
  Curly -> 3
  Angle -> 4
