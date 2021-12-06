{-# LANGUAGE GADTs #-}

module Aoc.Counter
  ( Counter,
    empty,
    foldl,
    count,
    add,
    total,
  )
where

import Dict (Dict)
import qualified Dict

newtype Counter a = Counter (Dict a Int)

empty :: Counter a
empty = Counter Dict.empty

foldl :: (a -> Int -> b -> b) -> b -> Counter a -> b
foldl f acc (Counter counter) = Dict.foldl f acc counter

count :: Ord a => List a -> Counter a
count = List.foldl (\item -> add item 1) empty

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
total (Counter counter) =
  counter
    |> Dict.values
    |> List.sum
