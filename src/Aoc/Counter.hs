module Aoc.Counter
  ( Counter,
    empty,
    foldl,
    fromList,
    add,
    total,
    toList,
    max,
    all,
    get,
  )
where

import qualified Data.List
import Dict (Dict)
import qualified Dict
import NriPrelude hiding (max)
import Prelude (flip)

newtype Counter a = Counter (Dict a Int)

empty :: Counter a
empty = Counter Dict.empty

foldl :: (a -> Int -> b -> b) -> b -> Counter a -> b
foldl f acc (Counter counter) = Dict.foldl f acc counter

fromList :: Ord a => List a -> Counter a
fromList = List.foldl (flip add 1) empty

all :: (Int -> Bool) -> Counter a -> Bool
all p (Counter counter) =
  Dict.filter (\_ -> not << p) counter
    |> Dict.isEmpty

get :: Ord a => a -> Counter a -> Maybe Int
get k (Counter counter) = Dict.get k counter

add :: Ord a => a -> Int -> Counter a -> Counter a
add k new (Counter counter) =
  Dict.update
    k
    ( \case
        Just old -> Just (old + new)
        Nothing -> Just new
    )
    counter
    |> Counter

total :: Counter a -> Int
total (Counter counter) = List.sum (Dict.values counter)

toList :: Counter a -> List (a, Int)
toList (Counter counter) = Dict.toList counter

max :: Counter a -> Maybe (a, Int)
max counter =
  case toList counter of
    [] -> Nothing
    xs ->
      Data.List.maximumBy (\(_, a) (_, b) -> compare a b) xs
        |> Just
