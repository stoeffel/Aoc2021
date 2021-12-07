module Aoc.Counter
  ( Counter,
    empty,
    foldl,
    fromList,
    add,
    total,
    toList,
    max,
  )
where

import qualified Data.List
import Dict (Dict)
import qualified Dict
import NriPrelude hiding (max)

newtype Counter a = Counter (Dict a Int)

empty :: Counter a
empty = Counter Dict.empty

foldl :: (a -> Int -> b -> b) -> b -> Counter a -> b
foldl f acc (Counter counter) = Dict.foldl f acc counter

fromList :: Ord a => List a -> Counter a
fromList = List.foldl (\item -> add item 1) empty

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
