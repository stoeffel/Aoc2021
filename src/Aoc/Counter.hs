module Aoc.Counter
  ( Counter,
    empty,
    singleton,
    foldl,
    fromList,
    add,
    keys,
    remove,
    total,
    toList,
    max,
    min,
    any,
    get,
    member,
    isEmpty,
    union,
    filterMap,
  )
where

import qualified Data.List
import qualified Data.Map.Strict
import Dict (Dict)
import qualified Dict
import NriPrelude hiding (max, min)
import Prelude (flip)

newtype Counter a = Counter (Dict a Int)
  deriving (Eq, Show)

empty :: Counter a
empty = Counter Dict.empty

isEmpty :: Counter a -> Bool
isEmpty (Counter dict) = Dict.isEmpty dict

singleton :: a -> Int -> Counter a
singleton a i = Counter (Dict.singleton a i)

keys :: Counter a -> List a
keys (Counter dict) = Dict.keys dict

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

union :: Ord a => Counter a -> Counter a -> Counter a
union (Counter counter1) (Counter counter2) =
  Data.Map.Strict.unionWith (+) counter1 counter2
    |> Counter

filterMap :: Ord b => (a -> Maybe b) -> Counter a -> Counter b
filterMap f (Counter counter) =
  Dict.foldl
    ( \k v acc ->
        case f k of
          Nothing -> acc
          Just b -> add b v acc
    )
    empty
    counter
