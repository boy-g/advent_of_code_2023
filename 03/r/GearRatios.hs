module Main (main) where

import Data.Char (isNumber)
import Debug.Trace (trace)

readStdin :: IO String
readStdin = getContents

--TODO data Map = Map [[Char]]

data Coord = Coord {
  x :: Int,
  y :: Int
} deriving (Show, Eq)

data NumCoorded = NumCoorded {
  numstr :: [Char],
  coords :: [Coord]
} deriving (Show, Eq)

emptyNumCoorded :: NumCoorded
emptyNumCoorded = NumCoorded {numstr="", coords=[]}

main :: IO ()
main = do
  puzzleInput <- readStdin
  let themap = parsePuzzleInput puzzleInput
  --print $ locateDirectAdjacents $ removeNonNumberCoords $ listifyAllCoords themap
  print $ parseAllNumbersInMap themap

parseAllNumbersInMap :: [[Char]] -> [NumCoorded]
parseAllNumbersInMap themap =
  removeEmptyNumbers allNumbersPlusEmpties
  where
    allNumbersOnLines = map (parseAllNumbersOnLine themap) ys
    ys = [0..(length themap - 1)]
    allNumbersPlusEmpties = concat allNumbersOnLines

removeEmptyNumbers :: [NumCoorded] -> [NumCoorded]
removeEmptyNumbers ns =
  filter isNonEmptyNumber ns

isNonEmptyNumber :: NumCoorded -> Bool
isNonEmptyNumber (NumCoorded {numstr=numstr}) =
  0 < length numstr

parseAllNumbersOnLine :: [[Char]] -> Int -> [NumCoorded]
parseAllNumbersOnLine themap y =
  foldr (parseNumberOnCoord themap y) [emptyNumCoorded] xs
  where
    theline = themap !! y
    xs      = [0..(length theline - 1)]

parseNumberOnCoord :: [[Char]] -> Int -> Int -> [NumCoorded] -> [NumCoorded]
parseNumberOnCoord themap y x (n : numCoordedOld)
  | startNew         = emptyNumCoorded : n : numCoordedOld
  | continueExisting = prepended : numCoordedOld
  | otherwise        = n : numCoordedOld
  where
    startNew         = headNonEmpty && (not $ isNumber theChar)
    headNonEmpty     = n /= emptyNumCoorded
    theChar          = (themap !! y) !! x
    continueExisting = isNumber theChar
    prepended        = prependToNumCoorded n x y theChar

prependToNumCoorded :: NumCoorded -> Int -> Int -> Char -> NumCoorded
prependToNumCoorded old x y c =
  new
  where
    new       = NumCoorded {numstr=numstrNew, coords=coordsNew}
    numstrNew = c : numstrOld
    NumCoorded {numstr=numstrOld, coords=coordsOld} = old
    coordsNew = Coord {x=x, y=y} : coordsOld

removeNonNumberCoords :: ([[Char]], [Coord]) -> ([[Char]], [Coord])
removeNonNumberCoords (themap, coordsOld) =
  (themap, coordsNumbers)
  where
    coordsNumbers = filter (isNumCoord themap) coordsOld

isNumCoord :: [[Char]] -> Coord -> Bool
isNumCoord themap (Coord {x, y}) =
  isNumber mapDataAtCoord
  where
    mapDataAtCoord = (themap !! y) !! x

listifyAllCoords :: [[Char]] -> ([[Char]], [Coord])
listifyAllCoords themap =
  (themap, coordsAll)
  where
    height = length themap
    width  = length $ head themap
    ys = [0..(height-1)]
    xs = [0..(width-1)]
    coordsAll         =
      [Coord {x=x, y=y} | x<-xs, y<-ys]

parsePuzzleInput :: String -> [[Char]]
parsePuzzleInput puzzleInput =
  lines puzzleInput

locateDirectAdjacents :: ([[Char]], [Coord]) -> ([[Char]], [Coord])
locateDirectAdjacents (themap, coordsAll) =
  (themap, coordsHasAdjacent)
  where
    coordsHasAdjacent = filter (hasAdjacentOnMap themap) coordsAll

hasAdjacentOnMap :: [[Char]] -> Coord -> Bool
hasAdjacentOnMap themap coord =
  0 /= length mapDataSymbols
  where
    coordsNeighborsAll    = genNeighborCoords coord
    coordsNeighborsActual = filter (isOnMap themap) coordsNeighborsAll
    mapDataNeighbors = map (getMapDataAt themap) coordsNeighborsActual
    mapDataSymbols   = filter isSymbol mapDataNeighbors

genNeighborCoords :: Coord -> [Coord]
genNeighborCoords (Coord {x, y}) =
  [
    Coord {x=x+1, y=y+0},
    Coord {x=x+1, y=y+1},
    Coord {x=x+0, y=y+1},
    Coord {x=x-1, y=y+1},
    Coord {x=x-1, y=y+0},
    Coord {x=x-1, y=y-1},
    Coord {x=x-0, y=y-1},
    Coord {x=x+1, y=y-1}
  ]

isOnMap :: [[Char]] -> Coord -> Bool
isOnMap themap (Coord {x, y}) =
  0 < x  &&
  0 < y  &&
  x < (length themap)  &&
  y < (length $ head themap)

getMapDataAt :: [[Char]] -> Coord -> Char
getMapDataAt themap (Coord {x, y}) =
  (themap !! y) !! x

isSymbol :: Char -> Bool
isSymbol c =
  (not $ isNumber c)  &&
  (not $ c == '.')
