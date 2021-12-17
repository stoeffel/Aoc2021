module Aoc.Day17 (solution) where

import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import qualified Control.Monad.Logic as L
import qualified Set
import Prelude (foldr, otherwise, pure, signum)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

-- |
-- Example: target area: x=153..199, y=-114..-75
parser :: P.Parser Target
parser = do
  _ <- P.string "target area: "
  _ <- P.string "x="
  minX <- P.decimal <|> P.signed P.decimal
  _ <- P.string ".."
  maxX <- P.decimal <|> P.signed P.decimal
  _ <- P.string ", y="
  minY <- P.decimal <|> P.signed P.decimal
  _ <- P.string ".."
  maxY <- P.decimal <|> P.signed P.decimal
  pure Target {minX, maxX, minY, maxY}

data Target = Target
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving (Show, Eq)

data Probe = Probe {x :: Int, y :: Int, vX :: Int, vY :: Int}
  deriving (Show, Eq)

solution1 :: Target -> Int
solution1 Target {minY, maxY} = minY * (minY + 1) // 2

solution2 :: Target -> Int
solution2 target =
  hitVelocity target
    |> L.observeAll
    |> Set.fromList
    |> Set.size

hitVelocity :: Target -> L.Logic (Int, Int)
hitVelocity target@Target {minX, maxX, minY, maxY} = do
  vX <- choose (List.range 1 maxX)
  vY <- choose (List.range minY (minY * (-1)))
  let probe = Probe {x = 0, y = 0, vX, vY}
  L.guard (isHit target probe)
  pure (vX, vY)

isHit :: Target -> Probe -> Bool
isHit target probe
  | isInTarget target probe = True
  | isOvershot target probe = False
  | otherwise = isHit target (step probe)

isInTarget :: Target -> Probe -> Bool
isInTarget Target {minX, maxX, minY, maxY} Probe {x, y} =
  minX <= x && x <= maxX && minY <= y && y <= maxY

isOvershot :: Target -> Probe -> Bool
isOvershot Target {minY} Probe {y} = y < minY

step :: Probe -> Probe
step Probe {x, y, vX, vY} =
  Probe
    { x = x + vX,
      y = y + vY,
      vX = vX - signum vX,
      vY = vY - 1
    }

choose :: List a -> L.Logic a
choose = foldr ((<|>) << pure) A.empty
