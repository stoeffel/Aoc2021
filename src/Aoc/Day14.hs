module Aoc.Day14 (solution) where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import Aoc.Parser ((<*))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import qualified Data.List
import Dict (Dict)
import qualified Dict
import Prelude (pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

data Input = Input
  { template :: List Char,
    rules :: Dict (Char, Char) Char
  }

parser :: P.Parser Input
parser = do
  template <- P.many P.letter
  _ <- P.count 2 P.endOfLine
  rules <- map Dict.fromList (P.lines ruleParser)
  pure Input {template, rules}

ruleParser :: P.Parser ((Char, Char), Char)
ruleParser = do
  a <- P.letter
  b <- P.letter
  _ <- P.string " -> "
  c <- P.letter
  pure ((a, b), c)

solution1 :: Input -> Maybe Int
solution1 = simulate 10

solution2 :: Input -> Maybe Int
solution2 = simulate 40

simulate :: Int -> Input -> Maybe Int
simulate n Input {template, rules} =
  pairs template
    |> Counter.fromList
    |> nTimes n (insert rules)
    |> split
    |> calcResult

calcResult :: Counter a -> Maybe Int
calcResult counted =
  Maybe.map2
    ( \(_, max) (_, min) ->
        ceiling (toFloat max / 2) - ceiling (toFloat min / 2)
    )
    (Counter.max counted)
    (Counter.min counted)

insert :: Ord a => Dict (a, a) a -> Counter (a, a) -> Counter (a, a)
insert rules =
  Counter.foldl
    ( \k@(a, b) n acc ->
        case Dict.get k rules of
          Nothing -> acc
          Just ins ->
            acc
              |> Counter.add (a, ins) n
              |> Counter.add (ins, b) n
    )
    Counter.empty

split :: Ord a => Counter (a, a) -> Counter a
split =
  Counter.foldl
    ( \(a, b) n acc ->
        acc
          |> Counter.add a n
          |> Counter.add b n
    )
    Counter.empty

pairs :: List a -> List (a, a)
pairs [] = []
pairs (x : xs) =
  List.foldl
    ( \next (prev, acc) ->
        (next, (prev, next) : acc)
    )
    (x, [])
    xs
    |> Tuple.second

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n - 1) f (f x)
