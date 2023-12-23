module Main (main) where


import Data.Char   (isNumber)
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

mapEmpty =
  Map []

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
    maps    = parseMaps puzzleInput

parseMaps :: String -> [Map]
parseMaps puzzleInput =
  maps
  where
    linesMaps = drop 2 $ lines puzzleInput
    maps      = foldl parseMapsLine [mapEmpty] linesMaps

parseMapsLine :: [Map] -> String -> [Map]
parseMapsLine mapsAcc lineMaps
  | lineMaps == "" =
    mapsAcc ++ [mapEmpty]
  | isNumber $ head lineMaps =
    parseMapsLineNumbers mapsAcc lineMaps
  | otherwise =
    mapsAcc

parseMapsLineNumbers :: [Map] -> String -> [Map]
parseMapsLineNumbers mapsAcc lineMaps =
  init mapsAcc ++ [mapAppended]
  where
    Map conversionsOld = last mapsAcc
    conversionNew      = parseConversion lineMaps
    mapAppended        = Map $ conversionsOld ++ [conversionNew]

parseConversion :: String -> Conversion
parseConversion line =
  Conversion cDestination cSource cLength
  where
    cDestination = read $ words line !! 0
    cSource      = read $ words line !! 1
    cLength      = read $ words line !! 2

parseSeeds :: String -> [Int]
parseSeeds puzzleInput =
  seeds
  where
    firstLine   = head $ lines puzzleInput
    seedsString = tail $ words firstLine
    seeds       = map read seedsString
