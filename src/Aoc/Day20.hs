module Aoc.Day20 (solution) where

import Aoc.Bits (Bits)
import qualified Aoc.Bits as Bits
import Aoc.Grid (Grid)
import qualified Aoc.Grid as Grid
import Aoc.Parser ((*>), (<*))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Array (Array)
import qualified Array
import Prelude (flip, otherwise, pure)

data Input = Input
  { enhancement :: Array Px,
    inputImage :: Grid Px
  }
  deriving (Show)

data Px = Light | Dark
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

parser :: P.Parser Input
parser = do
  enhancement <- map Array.fromList (P.many1 pxParser)
  _ <- P.endOfLine
  _ <- P.endOfLine
  inputImage <- map Grid.fromLists (P.lines (P.many1 pxParser))
  pure Input {enhancement, inputImage}

pxParser :: P.Parser Px
pxParser =
  P.oneOf
    [ P.char '#' *> pure Light,
      P.char '.' *> pure Dark
    ]

solution1 :: Input -> Int
solution1 Input {inputImage, enhancement} =
  inputImage
    |> Grid.extend Dark 2
    |> nTimes 2 (algo enhancement)
    |> Grid.filter (\_ x -> x == Light)
    |> Grid.toLists identity
    |> List.concat
    |> List.filterMap identity
    |> List.length

solution2 :: Input -> Int
solution2 Input {inputImage, enhancement} =
  inputImage
    |> Grid.extend Dark 50
    |> nTimes 50 (algo enhancement)
    |> Grid.filter (\_ x -> x == Light)
    |> Grid.toLists identity
    |> List.concat
    |> List.filterMap identity
    |> List.length

algo :: Array Px -> Int -> Grid Px -> Grid Px
algo enhancement n image =
  image
    |> Grid.foldWithKey
      ( \k v acc ->
          Grid.around k
            |> List.map (flip Grid.get image >> Maybe.withDefault def)
            |> pxToBits
            |> Bits.toInt
            |> flip Array.get enhancement
            |> Maybe.withDefault def
            |> (\n -> Grid.insert k n acc)
      )
      Grid.empty
  where
    def =
      if modBy 2 n == 0
        then Dark
        else Light

pxToBits :: List Px -> Bits
pxToBits =
  List.map <| \case
    Light -> Bits.One
    Dark -> Bits.Zero

nTimes :: Int -> (Int -> a -> a) -> a -> a
nTimes n f x = go 0 n f x
  where
    go i n f x
      | i == n = x
      | otherwise = go (i + 1) n f (f i x)
