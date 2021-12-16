{-# LANGUAGE ViewPatterns #-}

module Aoc.Day16 (solution) where

import Aoc.Bits (Bit (..), Bits)
import qualified Aoc.Bits as Bits
import Aoc.Parser ((*>))
import qualified Aoc.Parser as P
import qualified Aoc.Solution as S
import Data.List (splitAt)
import Prelude (fromIntegral, pure)

solution :: S.Solution
solution =
  S.Solution
    { S.parser,
      S.solution1,
      S.solution2,
      S.display = Debug.toString,
      S.visualize = Nothing
    }

parser :: P.Parser Bits
parser =
  P.many1 hexParser
    |> map List.concat

hexParser :: P.Parser Bits
hexParser =
  P.oneOf
    [ P.char '0' *> pure [Zero, Zero, Zero, Zero],
      P.char '1' *> pure [Zero, Zero, Zero, One],
      P.char '2' *> pure [Zero, Zero, One, Zero],
      P.char '3' *> pure [Zero, Zero, One, One],
      P.char '4' *> pure [Zero, One, Zero, Zero],
      P.char '5' *> pure [Zero, One, Zero, One],
      P.char '6' *> pure [Zero, One, One, Zero],
      P.char '7' *> pure [Zero, One, One, One],
      P.char '8' *> pure [One, Zero, Zero, Zero],
      P.char '9' *> pure [One, Zero, Zero, One],
      P.char 'A' *> pure [One, Zero, One, Zero],
      P.char 'B' *> pure [One, Zero, One, One],
      P.char 'C' *> pure [One, One, Zero, Zero],
      P.char 'D' *> pure [One, One, Zero, One],
      P.char 'E' *> pure [One, One, One, Zero],
      P.char 'F' *> pure [One, One, One, One]
    ]

solution1 :: Bits -> Maybe Int
solution1 bits =
  toPacket bits
    |> Maybe.map Tuple.first
    |> Maybe.map versionSum

solution2 :: Bits -> Maybe Int
solution2 bits =
  toPacket bits
    |> Maybe.map Tuple.first
    |> Maybe.andThen evaluate

data Packet = Packet
  { header :: Header,
    body :: Body
  }
  deriving (Eq, Show)

data Header = Header
  { version :: Int,
    typeId :: Int
  }
  deriving (Eq, Show)

data Body
  = LiteralValue Int
  | OperatorBody Operator
  deriving (Eq, Show)

data Operator = Operator
  { opType :: OpType,
    packets :: List Packet
  }
  deriving (Eq, Show)

data OpType = Op15 | Op11 deriving (Eq, Show)

evaluate :: Packet -> Maybe Int
evaluate Packet {header, body} =
  case body of
    LiteralValue n -> Just n
    OperatorBody op ->
      List.filterMap evaluate (packets op)
        |> typeIdToOp (typeId header)

typeIdToOp :: Int -> (List Int -> Maybe Int)
typeIdToOp typeId =
  case typeId of
    0 -> Just << List.sum
    1 -> Just << List.foldl (*) 1
    2 -> List.minimum
    3 -> List.maximum
    5 -> \case
      (x : y : _) -> Just (if x > y then 1 else 0)
      _ -> Just 0
    6 -> \case
      (x : y : _) -> Just (if x < y then 1 else 0)
      _ -> Just 0
    7 -> \case
      (x : y : _) -> Just (if x == y then 1 else 0)
      _ -> Just 0
    _ -> \_ -> Nothing

toPacket :: Bits -> Maybe (Packet, Bits)
toPacket bits =
  case toHeader bits of
    Just (header, restHeader) ->
      case toBody header restHeader of
        Just (body, restBody) -> Just (Packet {header, body}, restBody)
        Nothing -> Nothing
    Nothing -> Nothing

toHeader :: Bits -> Maybe (Header, Bits)
toHeader (a : b : c : d : e : f : bits) =
  Just
    ( Header
        { version = Bits.toInt [a, b, c],
          typeId = Bits.toInt [d, e, f]
        },
      bits
    )
toHeader _ = Nothing

toBody :: Header -> Bits -> Maybe (Body, Bits)
toBody _ [] = Nothing
toBody header bits =
  case typeId header of
    4 ->
      toLiteralValue bits
        |> Tuple.mapFirst (Bits.toInt >> LiteralValue)
        |> Just
    _ ->
      toOperator bits
        |> Maybe.map (Tuple.mapFirst OperatorBody)

toLiteralValue :: Bits -> (Bits, Bits)
toLiteralValue (splitAt 5 -> (prefix : nr, rest)) =
  case prefix of
    Zero -> (nr, rest)
    One ->
      toLiteralValue rest
        |> (\(next, remaining) -> (nr ++ next, remaining))

toOperator :: Bits -> Maybe (Operator, Bits)
toOperator bits =
  case bits of
    [] -> Nothing
    Zero : (splitAt 15 -> (lBits, op15)) ->
      let length = fromIntegral (Bits.toInt lBits)
          (bitsSubpackets, rest) = splitAt length op15
          (packets, _) = subPackets (\_ -> False) bitsSubpackets []
       in Just (Operator {opType = Op15, packets}, rest)
    One : (splitAt 11 -> (lBits, op11)) ->
      let length = Bits.toInt lBits
          (packets, rest) = subPackets (List.length >> (== length)) op11 []
       in Just (Operator {opType = Op11, packets}, rest)

subPackets :: (List Packet -> Bool) -> Bits -> List Packet -> (List Packet, Bits)
subPackets pred bits acc =
  if pred acc
    then (List.reverse acc, bits)
    else case toPacket bits of
      Just (packet, rest) -> subPackets pred rest (packet : acc)
      Nothing -> (List.reverse acc, bits)

versionSum :: Packet -> Int
versionSum Packet {header, body} =
  version header + case body of
    LiteralValue x -> 0
    OperatorBody Operator {packets} ->
      packets
        |> List.map versionSum
        |> List.sum
