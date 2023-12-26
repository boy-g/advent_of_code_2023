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
    handLabels :: [Label]
  }
  deriving (Show, Eq)

data Label =
  L2 |
  L3 |
  L4 |
  L5 |
  L6 |
  L7 |
  L8 |
  L9 |
  T  |
  J  |
  Q  |
  K  |
  A
  deriving (Show, Eq, Ord)

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
  totalWinnings
  where
    Game { gameLines=linesUnordered } = gameUnordered
    gameOrdered   = Game { gameLines=linesOrdered }
    linesOrdered  = sort linesUnordered
    linesRanked   = rankLines linesOrdered
    totalWinnings = calcTotalWinnings linesRanked

calcTotalWinnings :: [Line] -> Integer
calcTotalWinnings lines =
  sum $ map calcWinning lines

calcWinning :: Line -> Integer
calcWinning line =
  bid * rank
  where
    Bid bid          = lineBid line
    Just (Rank rank) = lineRank line

rankLines :: [Line] -> [Line]
rankLines linesUnranked =
  linesRanked
  where
    ranks         = [1..]
    linesAndRanks = zip linesUnranked ranks
    linesRanked   = map setRank linesAndRanks

setRank :: (Line, Integer) -> Line
setRank (lineIn, rank) =
  lineOut
  where
    lineOut = lineIn { lineRank = Just $ Rank rank }

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
    hand     = Hand $ castCharsToLabels $ head $ words inputLine
    bid      = Bid $ read $ last $ words inputLine

castCharsToLabels :: [Char] -> [Label]
castCharsToLabels cs =
  map castCharToLabel cs

castCharToLabel :: Char -> Label
castCharToLabel c
  | c == '2' = L2
  | c == '3' = L3
  | c == '4' = L4
  | c == '5' = L5
  | c == '6' = L6
  | c == '7' = L7
  | c == '8' = L8
  | c == '9' = L9
  | c == 'T' = T
  | c == 'J' = J
  | c == 'Q' = Q
  | c == 'K' = K
  | c == 'A' = A

lineCompare :: Line -> Line -> Ordering
lineCompare lineX lineY
  | handTypeX == handTypeY =
    compare handLabelsX handLabelsY
  | otherwise =
    compare handTypeX handTypeY
  where
    handTypeX   = getHandType $ lineHand lineX
    handTypeY   = getHandType $ lineHand lineY
    handLabelsX = handLabels $ lineHand lineX
    handLabelsY = handLabels $ lineHand lineY

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
    lengthSet = length $ fromList $ handLabels hand

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
