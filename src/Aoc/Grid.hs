{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Aoc.Grid
  ( Grid,
    Coord (..),
    -- Constructing / Deconstructing
    fromLists,
    fromCoords,
    toLists,
    -- Querying
    get,
    keys,
    values,
    coords,
    maxX,
    maxY,
    neightbours,
    surrounding,
    -- Modifying
    foldWithKey,
    map,
    mapKeys,
    update,
    filter,
    partition,
    union,
    -- From instances
    module Data.Foldable,
  )
where

import qualified Array
import Data.Foldable
import qualified Data.Map.Strict
import Dict (Dict)
import qualified Dict
import NriPrelude hiding (map)
import Prelude (Foldable, Functor, fmap)

newtype Grid a = Grid (Dict Coord a)
  deriving (Eq, Show, Functor, Foldable)

data Coord = Coord {x :: Int, y :: Int}
  deriving (Show, Eq, Ord)

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

toLists :: Grid a -> List (List (Maybe a))
toLists g =
  foldWithKey
    ( \Coord {x, y} v acc ->
        case Array.get y acc of
          Nothing -> acc
          Just row -> Array.set y (Array.set x (Just v) row) acc
    )
    ( Array.repeat (maxX g + 1) Nothing
        |> Array.repeat (maxY g + 1)
    )
    g
    |> Array.map Array.toList
    |> Array.toList

foldWithKey :: (Coord -> a -> b -> b) -> b -> Grid a -> b
foldWithKey f z (Grid d) = Dict.foldl f z d

map :: (a -> b) -> Grid a -> Grid b
map = fmap

coords :: Grid a -> List Coord
coords = foldWithKey (\c _ -> (:) c) []

get :: Coord -> Grid a -> Maybe a
get c (Grid d) = Dict.get c d

update :: Coord -> (a -> a) -> Grid a -> Grid a
update c f (Grid d) = Grid (Dict.update c (Maybe.map f) d)

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

neightbours :: Coord -> List Coord
neightbours coord@Coord {x, y} =
  [ coord {y = y - 1},
    coord {x = x + 1},
    coord {y = y + 1},
    coord {x = x - 1}
  ]

mapKeys :: (Coord -> Coord) -> Grid a -> Grid a
mapKeys f (Grid d) = Grid (Data.Map.Strict.mapKeys f d)

keys :: Grid a -> List Coord
keys (Grid d) = Dict.keys d

values :: Grid a -> List a
values (Grid d) = Dict.values d

partition :: (Coord -> a -> Bool) -> Grid a -> (Grid a, Grid a)
partition f (Grid d) =
  Dict.partition f d
    |> Tuple.mapBoth Grid Grid

union :: Grid a -> Grid a -> Grid a
union (Grid d1) (Grid d2) = Grid (Dict.union d1 d2)

maxX :: Grid a -> Int
maxX g =
  List.foldl (\Coord {x} -> max x) 0 (keys g)

maxY :: Grid a -> Int
maxY g =
  List.foldl (\Coord {y} -> max y) 0 (keys g)
