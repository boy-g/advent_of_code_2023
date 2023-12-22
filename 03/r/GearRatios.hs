module Main (main) where

import Data.Char (isNumber)
import Debug.Trace (trace)

readStdin :: IO String
readStdin = getContents

--TODO data Map = Map [[Char]]

data Coord = Coord {
  x :: Int,
  y :: Int
} deriving (Show)

main :: IO ()
main = do
  puzzleInput <- readStdin
  print $ locateAdjacents $ keepOnlyNumberCoordinates $ listifyAllCoordinates $ parsePuzzleInput puzzleInput

keepOnlyNumberCoordinates :: ([[Char]], [Coord]) -> ([[Char]], [Coord])
keepOnlyNumberCoordinates (themap, coordsOld) =
  (themap, coordsNumbers)
  where
    coordsNumbers = filter (isNumCoord themap) coordsOld

isNumCoord :: [[Char]] -> Coord -> Bool
isNumCoord themap (Coord {x, y}) =
  isNumber mapDataAtCoord
  where
    mapDataAtCoord = (themap !! y) !! x

listifyAllCoordinates :: [[Char]] -> ([[Char]], [Coord])
listifyAllCoordinates themap =
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

locateAdjacents :: ([[Char]], [Coord]) -> ([[Char]], [Coord])
locateAdjacents (themap, coordsAll) =
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
