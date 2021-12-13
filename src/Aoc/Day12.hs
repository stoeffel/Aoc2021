module Aoc.Day12 (solution, run2, parser) where

import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import Control.Monad (when)
import qualified Control.Monad.Logic as L
import Prelude (foldr, pure)
import qualified Prelude

run2 :: List (Edge Cave) -> Int
run2 = solution2

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2, S.display = Debug.toString}

parser :: P.Parser (List (Edge Cave))
parser = P.lines (edgeParser caveParser)

edgeParser :: P.Parser a -> P.Parser (Edge a)
edgeParser elementParser = do
  from <- elementParser
  _ <- P.char '-'
  to <- elementParser
  pure (Edge {from, to})

caveParser :: P.Parser Cave
caveParser = do
  name <- map Text.fromList (P.many1 P.letter)
  pure
    ( Cave
        { name,
          size = if Text.toUpper name == name then Big else Small
        }
    )

solution1 :: List (Edge Cave) -> Int
solution1 edges =
  edges
    |> pathLogic
      (Cave "start" Small)
      []
      ( \cave visited ->
          case size cave of
            Big -> pure ()
            Small ->
              if List.member cave visited
                then A.empty
                else pure ()
      )
    |> L.observeAll
    |> List.length

solution2 :: List (Edge Cave) -> Int
solution2 edges =
  edges
    |> pathLogic
      (Cave "start" Small)
      []
      ( \cave visited ->
          case size cave of
            Big -> pure ()
            Small ->
              case List.member cave visited of
                False -> pure ()
                True ->
                  if List.filter
                    (\Cave {size} -> size == Small)
                    visited
                    |> Counter.fromList
                    |> Counter.all (== 1)
                    then pure ()
                    else A.empty
      )
    |> L.observeAll
    |> List.length

data Edge a = Edge
  { from :: a,
    to :: a
  }
  deriving (Eq, Ord, Show)

type Path a = List (Edge a)

data Cave = Cave
  { name :: Text,
    size :: Size
  }
  deriving (Ord, Eq, Show)

data Size = Big | Small
  deriving (Eq, Ord, Show)

pathLogic ::
  Cave ->
  List Cave ->
  (Cave -> List Cave -> L.Logic ()) ->
  List (Edge Cave) ->
  L.Logic (List Cave)
pathLogic source visited guard edges = do
  next <- adjacentLogic source edges
  if name next == "end"
    then pure (next : visited)
    else do
      when (name next == "start") A.empty
      _ <- guard next visited
      pathLogic next (next : visited) guard edges

adjacentLogic :: Eq a => a -> List (Edge a) -> L.Logic a
adjacentLogic source edges = do
  Edge {from, to} <- choose edges
  if source == from
    then pure to
    else
      if source == to
        then pure from
        else A.empty

choose :: List a -> L.Logic a
choose = foldr ((<|>) << pure) A.empty
