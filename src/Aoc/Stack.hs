{-# LANGUAGE DeriveFoldable #-}

module Aoc.Stack (Stack, empty, isEmpty, push, pop, Data.Foldable.foldl) where

import qualified Data.Foldable
import Prelude (Foldable)

data Stack a = Stack [a]
  deriving (Eq, Show, Foldable)

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)
