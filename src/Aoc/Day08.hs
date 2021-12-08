module Aoc.Day08 (solution) where

import Aoc.Parser ((*>), (<*))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.List
import Prelude (flip, pure, traverse)

solution :: S.Solution
solution = S.Solution {S.parser, S.solution1, S.solution2}

data Entry = Entry
  { signalPattern :: List Signal,
    output :: List Signal
  }

-- | A signal contains a list of enabled segments.
--  AAAA
-- B    C
-- B    C
-- B    C
--  DDDD
-- E    F
-- E    F
-- E    F
--  GGGG
type Signal = List Segment

data Segment = A | B | C | D | E | F | G
  deriving (Eq, Ord)

parser :: P.Parser (List Entry)
parser = P.lines entryParser
  where
    entryParser :: P.Parser Entry
    entryParser = do
      signalPattern <- P.count 10 (signalParser <* P.space)
      _ <- P.char '|'
      output <- P.count 4 (P.space *> signalParser)
      pure Entry {signalPattern, output}

    signalParser :: P.Parser Signal
    signalParser = P.many1 segmentParser

    segmentParser :: P.Parser Segment
    segmentParser =
      P.keywords
        [ ("a", A),
          ("b", B),
          ("c", C),
          ("d", D),
          ("e", E),
          ("f", F),
          ("g", G)
        ]

solution1 :: List Entry -> Int
solution1 entries =
  entries
    |> List.concatMap (output >> List.filterMap getUniqueSignals)
    |> List.length

solution2 :: List Entry -> Int
solution2 entries =
  List.filterMap decodeEntry entries
    |> List.sum

getUniqueSignals :: Signal -> Maybe (Int, Signal)
getUniqueSignals signal =
  case List.length signal of
    2 -> Just (1, signal)
    3 -> Just (7, signal)
    4 -> Just (4, signal)
    7 -> Just (8, signal)
    _ -> Nothing

decodeEntry :: Entry -> Maybe Int
decodeEntry Entry {signalPattern, output} =
  [A, B, C, D, E, F, G]
    |> Data.List.permutations
    |> List.filter (findMapping signalPattern)
    |> List.filterMap (decodeOutput output)
    |> List.head

findMapping :: List Signal -> Signal -> Bool
findMapping signalPattern mapping =
  List.filterMap getUniqueSignals signalPattern
    |> List.all (\(k, v) -> decodeSignal mapping v == Just k)

decodeOutput :: List Signal -> Signal -> Maybe Int
decodeOutput output mapping =
  traverse (decodeSignal mapping) output
    |> Maybe.andThen joinNumbers

decodeSignal :: Signal -> Signal -> Maybe Int
decodeSignal mapping signal =
  signal
    |> List.map (mapSegment mapping)
    |> Data.List.sort
    |> signalToNumber

signalToNumber :: Signal -> Maybe Int
signalToNumber = \case
  [A, B, C, E, F, G] -> Just 0
  [C, F] -> Just 1
  [A, C, D, E, G] -> Just 2
  [A, C, D, F, G] -> Just 3
  [B, C, D, F] -> Just 4
  [A, B, D, F, G] -> Just 5
  [A, B, D, E, F, G] -> Just 6
  [A, C, F] -> Just 7
  [A, B, C, D, E, F, G] -> Just 8
  [A, B, C, D, F, G] -> Just 9
  _ -> Nothing

mapSegment :: Signal -> Segment -> Segment
mapSegment [a, b, c, d, e, f, g] = \case
  A -> a
  B -> b
  C -> c
  D -> d
  E -> e
  F -> f
  G -> g

joinNumbers :: List Int -> Maybe Int
joinNumbers numbers =
  List.map Text.fromInt numbers
    |> Text.join ""
    |> Text.toInt
