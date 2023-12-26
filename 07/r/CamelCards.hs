module Main (main) where


data Game =
  Game {
    gameHandBids :: [HandBid]
  }
  deriving (Show)

data HandBid =
  HandBid (Hand, Bid)
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
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> Game
parsePuzzleInput puzzleInput =
  Game {gameHandBids=handBids}
  where
    handBids    = map parseLine puzzleLines
    puzzleLines = lines puzzleInput

parseLine :: String -> HandBid
parseLine line =
  handbid
  where
    handbid = HandBid (Hand hand, Bid bid)
    hand    = head $ words line
    bid     = read $ last $ words line
