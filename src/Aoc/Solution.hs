{-# LANGUAGE FunctionalDependencies #-}

module Aoc.Solution where

import Data.Typeable (Typeable, typeOf)

class (Typeable a, Show b) => Solution a b | a -> b where
  solution1 :: a -> Text -> b
  solution2 :: a -> Text -> b
  name :: a -> Text
  name x = Debug.toString (typeOf x)
