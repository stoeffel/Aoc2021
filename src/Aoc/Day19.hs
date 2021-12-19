module Aoc.Day19 where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative ((<*), (<|>))
import qualified Control.Applicative as A
import qualified Control.Monad.Logic as L
import Dict (Dict)
import qualified Dict
import qualified Set
import Prelude (Show (..), foldr, pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

data Scanner = Scanner
  { coord :: Maybe Coord,
    beacons :: List Coord
  }
  deriving (Eq, Show)

data Coord = Coord
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Eq, Ord)

instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

-- | Parser
--  Example:
--
--  --- scanner 0 ---
--  404,-588,-901
--  528,-643,409
--
--  --- scanner 1 ---
--  686,422,578
--  605,423,415
parser :: P.Parser (Scanner, Dict Int Scanner)
parser = do
  origin <-
    map
      Tuple.second
      ( scannerParser
          (Just Coord {x = 0, y = 0, z = 0})
      )
  _ <- P.endOfLine
  scanners <- P.lines (scannerParser Nothing)
  pure (origin, Dict.fromList scanners)

-- | Parser
--  Example:
--
--  --- scanner 0 ---
--  404,-588,-901
--  528,-643,409
scannerParser :: Maybe Coord -> P.Parser (Int, Scanner)
scannerParser coord = do
  P.string "--- scanner "
  i <- P.decimal
  P.string " ---"
  P.endOfLine
  beacons <- P.lines coordParser
  pure (i, Scanner {coord, beacons})

coordParser :: P.Parser Coord
coordParser = do
  x <- P.decimal <|> P.signed P.decimal
  P.char ','
  y <- P.decimal <|> P.signed P.decimal
  P.char ','
  z <- P.decimal <|> P.signed P.decimal
  pure Coord {x, y, z}

solution1 :: (Scanner, Dict Int Scanner) -> Int
solution1 (origin, scanners) =
  findAllScanners
    (Dict.size scanners + 1)
    scanners
    (Dict.singleton 0 origin)
    |> Dict.values
    |> List.concatMap beacons
    |> Set.fromList
    |> Set.size

solution2 :: (Scanner, Dict Int Scanner) -> Int
solution2 (origin, scanners) =
  findAllScanners
    (Dict.size scanners + 1)
    scanners
    (Dict.singleton 0 origin)
    |> Dict.values
    |> List.filterMap coord
    |> manhattan
    |> L.observeAll
    |> List.maximum
    |> Maybe.withDefault 0

findAllScanners :: Int -> Dict Int Scanner -> Dict Int Scanner -> Dict Int Scanner
findAllScanners n scanners acc =
  let newAcc =
        Dict.foldl
          ( \i next acc ->
              case untilMaybe
                (overlap next >> L.observeT)
                (Dict.values acc) of
                Nothing -> acc
                Just new -> Dict.insert i new acc
          )
          acc
          scanners
   in if Dict.size newAcc == n
        then newAcc
        else
          findAllScanners
            n
            (Dict.remove 0 (Dict.diff scanners newAcc))
            newAcc

untilMaybe :: (a -> Maybe b) -> List a -> Maybe b
untilMaybe _ [] = Nothing
untilMaybe f (x : xs) =
  case f x of
    Nothing -> untilMaybe f xs
    Just y -> Just y

data Axis = X | Y | Z deriving (Show, Eq)

rotate :: Axis -> Coord -> Coord
rotate X c@Coord {x, y, z} = c {y = (- z), z = y}
rotate Y c@Coord {x, y, z} = c {x = z, z = (- x)}
rotate Z c@Coord {x, y, z} = c {x = y, y = (- x), z}

spin :: List Axis -> Coord -> Coord
spin [] c = c
spin (x : xs) c = spin xs (rotate x c)

rotations :: List (List Axis)
rotations =
  [ [],
    [X],
    [Y],
    [Z],
    [X, X],
    [X, Y],
    [X, Z],
    [Y, X],
    [Y, Y],
    [Z, Y],
    [Z, Z],
    [X, X, X],
    [X, X, Y],
    [X, X, Z],
    [X, Y, X],
    [X, Y, Y],
    [X, Z, Z],
    [Y, X, X],
    [Y, Y, Y],
    [Z, Z, Z],
    [X, X, X, Y],
    [X, X, Y, X],
    [X, Y, X, X],
    [X, Y, Y, Y]
  ]

subCoord :: Coord -> Coord -> Coord
subCoord (Coord x1 y1 z1) (Coord x2 y2 z2) =
  Coord (x1 - x2) (y1 - y2) (z1 - z2)

addCoord :: Coord -> Coord -> Coord
addCoord (Coord x1 y1 z1) (Coord x2 y2 z2) =
  Coord (x1 + x2) (y1 + y2) (z1 + z2)

overlap :: Scanner -> Scanner -> L.LogicT Maybe Scanner
overlap a b = do
  bb <- choose (beacons b)
  rot <- choose rotations
  ba <- choose (beacons a)
  let offset = subCoord bb (spin rot ba)
  let ba' = List.map (addCoord offset << spin rot) (beacons a)
  L.guard
    ( Set.intersect
        (Set.fromList ba')
        (Set.fromList (beacons b))
        |> Set.size
        |> (>= 12)
    )
  pure
    Scanner
      { coord = Just offset,
        beacons = ba'
      }

manhattan :: List Coord -> L.Logic Int
manhattan coords = do
  a <- choose coords
  b <- choose coords
  L.guard (a /= b)
  pure (manhattanDistance a b)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (Coord x1 y1 z1) (Coord x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

choose :: List a -> L.LogicT m a
choose = foldr ((<|>) << pure) A.empty
