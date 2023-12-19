module Main (main) where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import Text.Regex (mkRegex, subRegex)

data Hand = Hand {
  red   :: Integer,
  green :: Integer,
  blue  :: Integer
} deriving (Show)

data Game = Game {
  id    :: Integer,  -- TODO rename "gameId"?
  hands :: [Hand]
} deriving (Show)

readStdin :: IO String
readStdin = getContents

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

main :: IO ()
main =
  do
    puzzleInput <- readStdin
    print $ parseInput puzzleInput

parseInput :: String -> [Game]
parseInput input =
  map parseLine inputLines
  where
    inputLines = lines input

parseLine :: String -> Game
parseLine line =
  Game gameId gameHands
  where
    gameId    = parseGameId line
    gameHands = parseGameHands line

parseGameId :: String -> Integer
parseGameId line =
  gameId
  where
    lineWoHead       = subRegex (mkRegex "Game ") line ""
    lineWoHeadWoTail = subRegex (mkRegex ":.*") lineWoHead ""
    gameId           = read lineWoHeadWoTail

parseGameHands :: String -> [Hand]
parseGameHands line =
  hands
  where
    handsLine = subRegex (mkRegex ".*:") line ""
    handLines = splitOn ";" handsLine
    hands = map parseGameHand handLines

parseGameHand :: String -> Hand
parseGameHand handLine =
  Hand {red = red, green = green, blue = blue}
  where
    redLine   = safeHead $ filter (isInfixOf "red") colorLines
    red       = parseColorLine redLine
    greenLine = safeHead $ filter (isInfixOf "green") colorLines
    green     = parseColorLine greenLine
    blueLine  = safeHead $ filter (isInfixOf "blue") colorLines
    blue      = parseColorLine blueLine
    colorLines = splitOn "," handLine

parseColorLine :: Maybe String -> Integer
parseColorLine Nothing = 0
parseColorLine (Just line) =
  read lineWoHeadWoTail
  where
    lineWoHead       = subRegex (mkRegex "^ *") line ""
    lineWoHeadWoTail = subRegex (mkRegex " .*") lineWoHead ""
