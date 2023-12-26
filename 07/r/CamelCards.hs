module Main (main) where


import Data.List   (sort, isInfixOf)
import Data.Set    (fromList)
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
  deriving (Show, Eq)

instance Ord Line where
  compare = lineCompare

data Rank =
  Rank Integer
  deriving (Show, Eq)

data Hand =
  Hand {
    handLabels :: [Char]
  }
  deriving (Show, Eq)

data Bid =
  Bid Integer
  deriving (Show, Eq)

data HandType =
  HighCard     |
  OnePair      |
  TwoPair      |
  ThreeOfAKind |
  FullHouse    |
  FourOfAKind  |
  FiveOfAKind
  deriving (Eq, Ord)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

solvePuzzle :: Game -> Integer
solvePuzzle gameUnordered =
  trace (show gameOrdered) 0  -- TODO
  where
    Game { gameLines=linesUnordered } = gameUnordered
    linesOrdered = sort linesUnordered
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

lineCompare :: Line -> Line -> Ordering
lineCompare lineX lineY
  | handTypeX == handTypeY =
    EQ  -- TODO EQ not possible, resolve
  | otherwise =
    compare handTypeX handTypeY
  where
    handTypeX = getHandType $ lineHand lineX
    handTypeY = getHandType $ lineHand lineY

getHandType :: Hand -> HandType
getHandType hand
  | lengthSet == 5 =
    HighCard
  | lengthSet == 4 =
    OnePair
  | lengthSet == 3 && not (hasThrees hand) =
    TwoPair
  | lengthSet == 3 =
    ThreeOfAKind
  | lengthSet == 2 && not (hasFours hand) =
    FullHouse
  | lengthSet == 2 =
    FourOfAKind
  | lengthSet == 1 =
    FiveOfAKind
  | otherwise =
    error $ "getHandType: " ++ show hand
  where
    lengthSet  = length $ fromList $ handLabels hand

hasThrees :: Hand -> Bool
hasThrees hand =
  has
  where
    has             = hasFirst || hasSecond || hasThird
    labelsOrdered   = sort labelsUnordered
    labelsUnordered = handLabels hand
    threeFirst      = take 3 $ repeat $ labelsOrdered !! 0
    threeSecond     = take 3 $ repeat $ labelsOrdered !! 1
    threeThird      = take 3 $ repeat $ labelsOrdered !! 2
    hasFirst        = isInfixOf threeFirst labelsOrdered
    hasSecond       = isInfixOf threeSecond labelsOrdered
    hasThird        = isInfixOf threeThird labelsOrdered

hasFours :: Hand -> Bool
hasFours hand =
  has
  where
    has             = hasFirst || hasSecond
    hasFirst        = isInfixOf fourFirst labelsOrdered
    hasSecond       = isInfixOf fourSecond labelsOrdered
    fourFirst       = take 4 $ repeat $ labelsOrdered !! 0
    fourSecond      = take 4 $ repeat $ labelsOrdered !! 1
    labelsOrdered   = sort labelsUnordered
    labelsUnordered = handLabels hand
