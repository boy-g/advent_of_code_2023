module Main (main) where

import Data.Bits (shiftL)
import Data.List (intersect)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

data Card = Card {
  cardWins  :: [Int],
  cardHaves :: [Int]
} deriving (Show)

readStdin :: IO String
readStdin =
  getContents

main :: IO ()
main = do
  puzzleInput <- readStdin
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

solvePuzzle :: [Card] -> Int
solvePuzzle cards =
  trace (show pointss) $ sum pointss
  where
    winningNumberss = map findWinningNumbers cards
    pointss         = map calcPoints winningNumberss

calcPoints :: [Int] -> Int
calcPoints [] =
  0
calcPoints winningNumbers =
  shiftL 1 (length winningNumbers - 1)

findWinningNumbers :: Card -> [Int]
findWinningNumbers (Card {cardWins=wins, cardHaves=haves}) =
  intersect wins haves

parsePuzzleInput :: String -> [Card]
parsePuzzleInput puzzleInput =
  map parseLineCard puzzleLines
  where
    puzzleLines = lines puzzleInput

parseLineCard :: String -> Card
parseLineCard line =
  Card {cardWins=wins, cardHaves=haves}
  where
    lineWoPrefix   = last $ splitOn ":" line
    linesWinsHaves = splitOn "|" lineWoPrefix
    winsStringsWSpaces  = splitOn " " $ head linesWinsHaves
    havesStringsWSpaces = splitOn " " $ last linesWinsHaves
    winsStrings  = filter isNonEmptyString winsStringsWSpaces
    havesStrings = filter isNonEmptyString havesStringsWSpaces
    wins  = map read winsStrings :: [Int]
    haves = map read havesStrings :: [Int]

isNonEmptyString :: String -> Bool
isNonEmptyString s =
  0 < length s
