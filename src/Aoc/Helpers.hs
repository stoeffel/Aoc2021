{-# LANGUAGE AllowAmbiguousTypes #-}

module Aoc.Helpers where

import Data.Typeable (Typeable, typeOf)

class Typeable a => Solution a where
  solution1 :: a -> Text -> Text
  solution1 _ x = x
  solution2 :: a -> Text -> Text
  solution2 _ x = x
  name :: a -> Text
  name x = Debug.toString (typeOf x)

count :: Eq a => a -> List a -> Int
count toCount = List.filter (== toCount) >> List.length
