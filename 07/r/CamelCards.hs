module Main (main) where


import Data.List   (sort)
import Debug.Trace (trace)


data Game =
  Game {
    gameLines :: [Line]
  }
  deriving (Show)

data Line =
  Line {
    lineHand :: Hand,
    lineBid  :: Bid,
    lineRank :: Maybe Rank
  }
  deriving (Show)

--TODO instance Line Ord

data Rank =
  Rank Integer
  deriving (Show)

data Hand =
  Hand {
    handLabels :: [Char]
  }
  deriving (Show)

data Bid =
  Bid Integer
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

solvePuzzle :: Game -> Integer
solvePuzzle gameUnordered =
  trace (show gameOrdered) 0  -- TODO
  where
    Game { gameLines=lines } = gameUnordered
    linesOrdered = lines  --TODO sort lines
    gameOrdered = Game { gameLines=linesOrdered }

parsePuzzleInput :: String -> Game
parsePuzzleInput puzzleInput =
  Game {gameLines=line}
  where
    line        = map parseLine puzzleLines
    puzzleLines = lines puzzleInput

parseLine :: String -> Line
parseLine inputLine =
  gameLine
  where
    gameLine = Line { lineHand=hand, lineBid=bid, lineRank=Nothing }
    hand     = Hand $ head $ words inputLine
    bid      = Bid $ read $ last $ words inputLine
