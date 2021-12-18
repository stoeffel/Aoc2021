module Aoc.Day18 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative ((<*), (<|>))
import qualified Control.Applicative as A
import qualified Control.Monad.Logic as L
import Prelude (flip, foldr, pure)

type Forest a = List (Tree a)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser (Tree Int, Forest Int)
parser = do
  tree <- treeParser <* P.endOfLine
  forest <- P.lines treeParser
  pure (tree, forest)

treeParser :: P.Parser (Tree Int)
treeParser = do
  P.char '['
  t <- nodeParser <|> treeParser
  P.char ']'
  pure t

nodeParser :: P.Parser (Tree Int)
nodeParser = do
  a <- map Leaf P.decimal <|> treeParser
  _ <- P.char ','
  b <- map Leaf P.decimal <|> treeParser
  pure (Node a b)

solution1 :: (Tree Int, Forest Int) -> Int
solution1 (tree, forest) =
  List.foldl (flip addTrees) tree forest
    |> magnitude

solution2 :: (Tree Int, Forest Int) -> Int
solution2 (tree, forest) =
  largestMagnitude (tree : forest)
    |> L.observeAll
    |> List.maximum
    |> Maybe.withDefault 0

magnitude :: Tree Int -> Int
magnitude = \case
  Leaf x -> x
  Node l r -> 3 * magnitude l + 2 * magnitude r

largestMagnitude :: Forest Int -> L.Logic Int
largestMagnitude forest = do
  a <- choose forest
  b <- choose forest
  L.guard (a /= b)
  addTrees a b
    |> magnitude
    |> pure

addTrees :: Tree Int -> Tree Int -> Tree Int
addTrees a b =
  Node a b
    |> flattenWithDepth 0
    |> reduce
    |> unflattenWithDepth 0
    |> Maybe.map Tuple.first
    |> Maybe.withDefault a

reduce :: List (Int, Int) -> List (Int, Int)
reduce xs =
  if List.any (\(d, _) -> d == 5) xs
    then reduce (explode xs)
    else
      if List.any (\(_, x) -> x >= 10) xs
        then reduce (split xs)
        else xs

split :: List (Int, Int) -> List (Int, Int)
split [] = []
split ((d, x) : xs) =
  if x >= 10
    then (d + 1, x // 2) : (d + 1, ceiling (toFloat x / 2)) : xs
    else (d, x) : split xs

explode :: List (Int, Int) -> List (Int, Int)
explode [] = []
explode ((5, x) : (5, y) : xs) =
  (4, 0) : mapHead (Tuple.mapSecond (+ y)) xs
explode ((d, x) : (5, y) : (5, z) : xs) =
  (d, x + y) : (4, 0) : mapHead (Tuple.mapSecond (+ z)) xs
explode (x : xs) = x : explode xs

mapHead :: (a -> a) -> List a -> List a
mapHead _ [] = []
mapHead f (x : xs) = f x : xs

flattenWithDepth :: Int -> Tree a -> List (Int, a)
flattenWithDepth ind (Leaf a) = [(ind, a)]
flattenWithDepth ind (Node l r) =
  flattenWithDepth (ind + 1) l ++ flattenWithDepth (ind + 1) r

unflattenWithDepth :: Int -> List (Int, a) -> Maybe (Tree a, List (Int, a))
unflattenWithDepth _ [] = Nothing
unflattenWithDepth d ((dx, x) : xs) =
  if d == dx
    then Just (Leaf x, xs)
    else case unflattenWithDepth (d + 1) ((dx, x) : xs) of
      Nothing -> Nothing
      Just (sn1, xs1) ->
        case unflattenWithDepth (d + 1) xs1 of
          Nothing -> Nothing
          Just (sn2, xs2) -> Just (Node sn1 sn2, xs2)

choose :: List a -> L.Logic a
choose = foldr ((<|>) << pure) A.empty
