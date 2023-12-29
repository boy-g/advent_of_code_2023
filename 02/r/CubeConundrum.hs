module Main (main) where

import Prelude hiding (id)

import Data.Char (isSpace, isNumber)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Text.Lazy (unpack)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import Text.Pretty.Simple (pShow, pShowNoColor, pPrint)
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

maxHand :: Hand
maxHand = Hand {red = 12, green = 13, blue = 14}

main :: IO ()
main =
  do
    puzzleInput <- readStdin
    print $ sum $ getGameIds $ filterPossibleGames $ parseInput puzzleInput
    --print $ tagGamePossibilities $ parseInput puzzleInput
    --pPrint $ tagGamePossibilities $ parseInput puzzleInput

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
  read lineWoSpaceWoLetters
  where
    lineWoSpace          = filter (not . isSpace) line
    lineWoSpaceWoLetters = filter isNumber lineWoSpace

filterPossibleGames :: [Game] -> [Game]
filterPossibleGames games = filter isGamePossible games

isGamePossible :: Game -> Bool
isGamePossible game =
  --trace (show game ++ " possible? " ++ show possible) possible
  possible
  where
    handsPossibilities = map isHandPossible $ hands game
    possible           = foldr (&&) True handsPossibilities

isHandPossible :: Hand -> Bool
isHandPossible hand =
  --trace (show hand ++ " hand? " ++ show possible) possible  -- TODO
  possible
  where
    possible =
      (red   hand <= red   maxHand)  &&
      (green hand <= green maxHand)  &&
      (blue  hand <= blue  maxHand)

getGameIds :: [Game] -> [Integer]
getGameIds games =
  map getGameId games

getGameId :: Game -> Integer
getGameId (Game {id = id}) =
  id

tagGamePossibilities :: [Game] -> [(Bool, Game)]
tagGamePossibilities games =
  map tagGamePossibility games

tagGamePossibility :: Game -> (Bool, Game)
tagGamePossibility game =
  (isGamePossible game, game)
