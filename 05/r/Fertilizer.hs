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
  print $ findLowestLocation $ parsePuzzleInput puzzleInput

findLowestLocation :: Almanac -> Int
findLowestLocation almanac =
  lowestLocation
  where
    lowestLocation = findLocation maps (head seeds)  --TODO all
    Almanac {almanacSeeds=seeds, almanacMaps=maps} = almanac

findLocation :: [Map] -> Int -> Int
findLocation maps seed =
  location
  where
    Map conversions = head maps
    location = convertRecurse conversions seed

convertRecurse :: [Conversion] -> Int -> Int
convertRecurse [] numCurrent =
  numCurrent
convertRecurse (conversion:cs) numCurrent
  | isInConversion conversion numCurrent =
    convertRecurse cs converted
  | otherwise =
    convertRecurse cs numCurrent
  where
    converted = convertOne conversion numCurrent

convertOne :: Conversion -> Int -> Int
convertOne conversion num =
  trace "convertOne: " converted
  where
    converted = num + offset
    offset    = destination - source
    Conversion destination source _ = conversion

isInConversion :: Conversion -> Int -> Bool
isInConversion conversion num =
  trace ("isInConversion: " ++ show rangeSource) isIn
  where
    isIn = (low <= num) && (num <= high)
    low  = rangeSource
    high = rangeSource + rangeLength - 1
    Conversion _ rangeSource rangeLength = conversion

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
