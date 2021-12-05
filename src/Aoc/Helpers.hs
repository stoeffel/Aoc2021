{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Aoc.Helpers where

import Data.Typeable (Typeable, typeOf)

class (Typeable a, Show b) => Solution a b | a -> b where
  solution1 :: a -> Text -> b
  solution2 :: a -> Text -> b
  name :: a -> Text
  name x = Debug.toString (typeOf x)

count :: Eq a => a -> List a -> Int
count toCount = List.filter (== toCount) >> List.length
