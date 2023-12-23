module Main (main) where

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
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> [Card]
parsePuzzleInput puzzleInput =
  [parseLineCard $ puzzleLines !! 0]  --TODO all
  where
    puzzleLines = lines puzzleInput

parseLineCard :: String -> Card
parseLineCard line =
  trace (show (wins, haves)) Card {cardWins=wins, cardHaves=haves}
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
