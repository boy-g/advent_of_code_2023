module Main (main) where


import Debug.Trace (trace)


data Almanac =
  Almanac {
    almanacSeeds :: [Int],
    almanacMaps  :: [Map]
  }
  deriving (Show)

data Map =
  Map [Conversion]
  deriving (Show)

data Conversion =
  Conversion {
    conversionDestination :: Int,
    conversionSource      :: Int,
    conversionLength      :: Int
  }
  deriving (Show)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> Almanac
parsePuzzleInput puzzleInput =
  almanac
  where
    almanac = Almanac seeds maps
    seeds   = parseSeeds puzzleInput
    maps    = []  --TODO

parseSeeds :: String -> [Int]
parseSeeds puzzleInput =
  seeds
  where
    firstLine   = head $ lines puzzleInput
    seedsString = tail $ words firstLine
    seeds       = map read seedsString
