module Aoc.Day22 where

import Aoc.Counter (Counter)
import qualified Aoc.Counter as Counter
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Prelude (pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

data Instruction = Instruction
  { power :: Power,
    cuboid :: Cuboid
  }
  deriving (Eq, Ord, Show)

data Power = On | Off
  deriving (Eq, Ord, Show)

data Cuboid = Cuboid
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int,
    minZ :: Int,
    maxZ :: Int
  }
  deriving (Eq, Ord, Show)

-- Examples
--    on x=-29..23,y=-30..15,z=-3..49
--    on x=-15..38,y=-36..10,z=-43..7
parser :: P.Parser (List Instruction)
parser = P.lines instructionParser

instructionParser :: P.Parser Instruction
instructionParser = do
  power <- powerParser
  P.string " "
  cuboid <- cuboidParser
  pure Instruction {power, cuboid}

powerParser :: P.Parser Power
powerParser =
  P.keywords
    [ ("on", On),
      ("off", Off)
    ]

cuboidParser :: P.Parser Cuboid
cuboidParser = do
  _ <- P.string "x="
  minX <- P.decimal <|> P.signed P.decimal
  _ <- P.string ".."
  maxX <- P.decimal <|> P.signed P.decimal
  _ <- P.string ","
  _ <- P.string "y="
  minY <- P.decimal <|> P.signed P.decimal
  _ <- P.string ".."
  maxY <- P.decimal <|> P.signed P.decimal
  _ <- P.string ","
  _ <- P.string "z="
  minZ <- P.decimal <|> P.signed P.decimal
  _ <- P.string ".."
  maxZ <- P.decimal <|> P.signed P.decimal
  pure Cuboid {minX, maxX, minY, maxY, minZ, maxZ}

solution1 :: List Instruction -> Int
solution1 instructions =
  instructions
    |> List.filter (not << cuboidsBetween (-50) 50)
    |> List.foldl followInstructions (Counter.empty, Counter.empty)
    |> total

solution2 :: List Instruction -> Int
solution2 instructions =
  instructions
    |> List.foldl followInstructions (Counter.empty, Counter.empty)
    |> total

total :: (Counter Cuboid, Counter Cuboid) -> Int
total (on, off) =
  Counter.foldl (\k v acc -> countCubes k * v + acc) 0 on
    - Counter.foldl (\k v acc -> countCubes k * v + acc) 0 off

followInstructions :: Instruction -> (Counter Cuboid, Counter Cuboid) -> (Counter Cuboid, Counter Cuboid)
followInstructions Instruction {power, cuboid} (on, off) =
  ( Counter.filterMap (union cuboid) off
      |> Counter.union on
      |> maybeTurnOn cuboid power,
    Counter.filterMap (union cuboid) on
      |> Counter.union off
  )

maybeTurnOn :: Cuboid -> Power -> Counter Cuboid -> Counter Cuboid
maybeTurnOn cuboid = \case
  On -> Counter.add cuboid 1
  Off -> identity

cuboidsBetween :: Int -> Int -> Instruction -> Bool
cuboidsBetween minPos maxPos Instruction {cuboid} =
  minX cuboid > maxPos
    || minY cuboid > maxPos
    || minZ cuboid > maxPos
    || maxX cuboid < minPos
    || maxY cuboid < minPos
    || maxZ cuboid < minPos

countCubes :: Cuboid -> Int
countCubes Cuboid {minX, maxX, minY, maxY, minZ, maxZ} =
  (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

union :: Cuboid -> Cuboid -> Maybe Cuboid
union a b = do
  let minX' = max (minX a) (minX b)
  let maxX' = min (maxX a) (maxX b)
  guard (minX' <= maxX')
  let minY' = max (minY a) (minY b)
  let maxY' = min (maxY a) (maxY b)
  guard (minY' <= maxY')
  let minZ' = max (minZ a) (minZ b)
  let maxZ' = min (maxZ a) (maxZ b)
  guard (minZ' <= maxZ')
  pure
    Cuboid
      { minX = minX',
        maxX = maxX',
        minY = minY',
        maxY = maxY',
        minZ = minZ',
        maxZ = maxZ'
      }
