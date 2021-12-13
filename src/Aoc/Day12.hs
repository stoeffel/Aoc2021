module Aoc.Day12 (solution) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import Control.Monad (when)
import qualified Control.Monad.Logic as L
import Prelude (foldr, pure)
import qualified Prelude

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
    |> pathLogic (Cave "start" Small) Counter.empty smallOnlyOnce
    |> L.observeAll
    |> List.length

solution2 :: List (Edge Cave) -> Int
solution2 edges =
  edges
    |> pathLogic (Cave "start" Small) Counter.empty oneSmallTwice
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

pathLogic :: Cave -> Counter Cave -> (Cave -> Counter Cave -> L.Logic ()) -> List (Edge Cave) -> L.Logic ()
pathLogic source visited guard edges = do
  next <- adjacentLogic source edges
  if name next == "end"
    then pure ()
    else do
      when (name next == "start") A.empty
      _ <- guard next visited
      pathLogic next (Counter.add next 1 visited) guard edges

adjacentLogic :: Eq a => a -> List (Edge a) -> L.Logic a
adjacentLogic source edges = do
  Edge {from, to} <- choose edges
  if source == from
    then pure to
    else
      if source == to
        then pure from
        else A.empty

smallOnlyOnce :: Cave -> Counter Cave -> L.Logic ()
smallOnlyOnce cave visited =
  case size cave of
    Big -> pure ()
    Small ->
      if Counter.member cave visited
        then A.empty
        else pure ()

oneSmallTwice :: Cave -> Counter Cave -> L.Logic ()
oneSmallTwice cave visited =
  case size cave of
    Big -> pure ()
    Small ->
      case Counter.member cave visited of
        False -> pure ()
        True ->
          if Counter.any
            (\Cave {size} v -> size == Small && v > 1)
            visited
            then A.empty
            else pure ()

choose :: List a -> L.Logic a
choose = foldr ((<|>) << pure) A.empty
