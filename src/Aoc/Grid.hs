{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Aoc.Grid
  ( Grid,
    Coord (..),
    -- Constructing / Deconstructing
    fromLists,
    fromCoords,
    toText,
    toLists,
    empty,
    -- Querying
    get,
    keys,
    coords,
    maxX,
    maxY,
    neighbors,
    surrounding,
    around,
    -- Modifying
    insert,
    foldWithKey,
    map,
    mapWithKey,
    mapKeys,
    update,
    filter,
    union,
    extend,
    -- From instances
    module Data.Foldable,
  )
where

import qualified Array
import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.Map.Strict
import Dict (Dict)
import qualified Dict
import NriPrelude hiding (map)
import Prelude (Foldable, Functor, fmap)

newtype Grid a = Grid (Dict Coord a)
  deriving (Eq, Show, Functor, Foldable)

data Coord = Coord {x :: Int, y :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Hashable Coord

fromLists :: List (List a) -> Grid a
fromLists xs =
  xs
    |> List.indexedMap
      ( \y ->
          List.indexedMap
            (\x v -> (Coord {x, y}, v))
      )
    |> List.concat
    |> Dict.fromList
    |> Grid

fromCoords :: List (Coord, a) -> Grid a
fromCoords xs =
  xs
    |> Dict.fromList
    |> Grid

empty :: Grid a
empty = Grid Dict.empty

toText :: (Maybe a -> Text) -> Grid a -> Text
toText toText grid =
  grid
    |> toLists identity
    |> List.map (Text.join "" << List.map toText)
    |> Text.join "\n"

toLists :: (Maybe a -> b) -> Grid a -> List (List b)
toLists toElement g =
  foldWithKey
    ( \Coord {x, y} v acc ->
        case Array.get y acc of
          Nothing -> acc
          Just row -> Array.set y (Array.set x (toElement (Just v)) row) acc
    )
    ( Array.repeat (maxX g + 1) (toElement Nothing)
        |> Array.repeat (maxY g + 1)
    )
    g
    |> Array.map Array.toList
    |> Array.toList

foldWithKey :: (Coord -> a -> b -> b) -> b -> Grid a -> b
foldWithKey f z (Grid d) = Dict.foldl f z d

map :: (a -> b) -> Grid a -> Grid b
map = fmap

mapWithKey :: (Coord -> a -> b) -> Grid a -> Grid b
mapWithKey f (Grid d) = Grid (Dict.map f d)

coords :: Grid a -> List Coord
coords = foldWithKey (\c _ -> (:) c) []

get :: Coord -> Grid a -> Maybe a
get c (Grid d) = Dict.get c d

update :: Coord -> (a -> a) -> Grid a -> Grid a
update c f (Grid d) = Grid (Dict.update c (Maybe.map f) d)

insert :: Coord -> a -> Grid a -> Grid a
insert c v (Grid d) = Grid (Dict.insert c v d)

filter :: (Coord -> a -> Bool) -> Grid a -> Grid a
filter f (Grid d) = Grid (Dict.filter f d)

surrounding :: Coord -> List Coord
surrounding coord@Coord {x, y} =
  [ coord {y = y - 1},
    coord {x = x + 1},
    coord {y = y + 1},
    coord {x = x - 1},
    coord {x = x - 1, y = y - 1},
    coord {x = x + 1, y = y - 1},
    coord {x = x - 1, y = y + 1},
    coord {x = x + 1, y = y + 1}
  ]

neighbors :: Coord -> List Coord
neighbors coord@Coord {x, y} =
  [ coord {y = y - 1},
    coord {x = x + 1},
    coord {y = y + 1},
    coord {x = x - 1}
  ]

around :: Coord -> List Coord
around coord@Coord {x, y} =
  [ coord {x = x - 1, y = y - 1},
    coord {y = y - 1},
    coord {x = x + 1, y = y - 1},
    coord {x = x - 1},
    coord,
    coord {x = x + 1},
    coord {x = x - 1, y = y + 1},
    coord {y = y + 1},
    coord {x = x + 1, y = y + 1}
  ]

mapKeys :: (Coord -> Coord) -> Grid a -> Grid a
mapKeys f g = foldWithKey (\c v -> insert (f c) v) empty g

keys :: Grid a -> List Coord
keys (Grid d) = Dict.keys d

maxX :: Grid a -> Int
maxX g =
  List.foldl (\Coord {x} -> max x) 0 (keys g)

maxY :: Grid a -> Int
maxY g =
  List.foldl (\Coord {y} -> max y) 0 (keys g)

union :: Grid a -> Grid a -> Grid a
union (Grid d1) (Grid d2) = Grid (Dict.union d1 d2)

extend :: a -> Int -> Grid a -> Grid a
extend v n g =
  toLists (Maybe.withDefault v) g
    |> List.map (extendLeft n v >> extendRight n v)
    |> extendLeft n row
    |> extendRight n row
    |> fromLists
  where
    row = List.repeat (maxX g + 1 + n * 2) v

extendLeft :: Int -> a -> List a -> List a
extendLeft n x xs = List.repeat n x ++ xs

extendRight :: Int -> a -> List a -> List a
extendRight n x xs = xs ++ List.repeat n x
