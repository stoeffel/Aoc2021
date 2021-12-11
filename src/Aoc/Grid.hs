{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Aoc.Grid
  ( Grid,
    Coord,
    fromLists,
    foldWithKey,
    map,
    coords,
    get,
    update,
    filter,
    module Data.Foldable,
    surrounding,
    neightbours,
  )
where

import Data.Foldable
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
      ( \x ->
          List.indexedMap
            (\y v -> (Coord {x, y}, v))
      )
    |> List.concat
    |> Dict.fromList
    |> Grid

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
