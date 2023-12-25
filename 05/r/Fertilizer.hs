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

mapEmpty :: Map
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
  trace ("findLowestLocation: " ++ show locations) lowestLocation
  where
    locations      = map (findLocation maps) seeds
    lowestLocation = head locations  --TODO minimum
    Almanac {almanacSeeds=seeds, almanacMaps=maps} = almanac

findLocation :: [Map] -> Int -> Int
findLocation maps seed =
  location
  where
    location = foldl convertViaMap seed maps

convertViaMap :: Int -> Map -> Int
convertViaMap numCurrent (Map conversions) =
  convertRecurse conversions numCurrent

convertRecurse :: [Conversion] -> Int -> Int
convertRecurse [] numCurrent =
  numCurrent
convertRecurse (conversion:cs) numCurrent
  | isInConversion conversion numCurrent =
    converted
  | otherwise =
    convertRecurse cs numCurrent
  where
    converted = convertOnce conversion numCurrent

convertOnce :: Conversion -> Int -> Int
convertOnce conversion num =
  --trace ("convertOnce: " ++ show num ++ " " ++ show converted) converted
  converted
  where
    converted = num + offset
    offset    = destination - source
    Conversion destination source _ = conversion

isInConversion :: Conversion -> Int -> Bool
isInConversion conversion num =
  --trace ("isIn: " ++ show num ++ " " ++ show conversion ++ " " ++ show isIn) isIn
  isIn
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
