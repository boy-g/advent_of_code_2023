module Main (main) where


data Report =
  Report {
    reportHistories:: [History]
  }
  deriving (Show)

data History =
  History {
    historyValues :: [Integer]
  }
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> Report
parsePuzzleInput puzzleInput = report
  where
  report    = Report histories
  histories = map parseLine $ lines puzzleInput

parseLine :: String -> History
parseLine line = history
  where
  history = History values
  values  = map read $ words line
