module Aoc.Counter
  ( Counter,
    empty,
    singleton,
    foldl,
    fromList,
    add,
    remove,
    total,
    toList,
    max,
    min,
    any,
    get,
    member,
    set,
    filter,
  )
where

import qualified Data.List
import Dict (Dict)
import qualified Dict
import NriPrelude hiding (max, min)
import Prelude (flip)

newtype Counter a = Counter (Dict a Int)
  deriving (Eq, Show)

empty :: Counter a
empty = Counter Dict.empty

singleton :: a -> Int -> Counter a
singleton a i = Counter (Dict.singleton a i)

foldl :: (a -> Int -> b -> b) -> b -> Counter a -> b
foldl f acc (Counter counter) = Dict.foldl f acc counter

fromList :: Ord a => List a -> Counter a
fromList = List.foldl (flip add 1) empty

any :: (a -> Int -> Bool) -> Counter a -> Bool
any p (Counter counter) =
  Dict.foldl
    ( \k v acc ->
        if acc
          then True
          else p k v
    )
    False
    counter

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

set :: Ord a => a -> Int -> Counter a -> Counter a
set k new (Counter counter) =
  Dict.update k (\_ -> Just new) counter
    |> Counter

remove :: Ord a => a -> Counter a -> Counter a
remove k (Counter counter) =
  Dict.remove k counter
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

min :: Counter a -> Maybe (a, Int)
min counter =
  case toList counter of
    [] -> Nothing
    xs ->
      Data.List.minimumBy (\(_, a) (_, b) -> compare a b) xs
        |> Just

member :: Ord a => a -> Counter a -> Bool
member k (Counter counter) = Dict.member k counter

filter :: (a -> Bool) -> Counter a -> Counter a
filter p (Counter counter) =
  Dict.filter (\k _ -> p k) counter
    |> Counter
