module Main (main) where


import Data.List   (genericIndex)
import Debug.Trace (trace)


data SketchOfPipes =
  SketchOfPipes [[Char]]

data Coord =
  Coord {
    coordX :: Integer,
    coordY :: Integer
  }
  deriving (Show, Eq)

data Direction =
  DirUp | DirDown | DirLeft | DirRight
  deriving (Eq)


main :: IO ()
main = do
  puzzleInput <- getContents
  print $ solvePuzzle $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> SketchOfPipes
parsePuzzleInput puzzleInput = sketch
  where
  sketch       = SketchOfPipes linesOfPipes
  linesOfPipes = lines puzzleInput

solvePuzzle :: SketchOfPipes -> Integer
--solvePuzzle sketch = trace ("solvePuzzle: " ++ show pathOfLoop) lengthHalf
solvePuzzle sketch = lengthHalf
  where
  coordOfStart  = getStart sketch
  pathOfInitial = [coordOfStart]
  pathOfLoop    = extendPath sketch pathOfInitial
  lengthHalf    = toInteger $ div (length pathOfLoop) 2

getStart :: SketchOfPipes -> Coord
getStart sketch = start
  where
  (width, height) = getWidthHeight sketch
  xs         = [0..(width - 1)]
  ys         = [0..(height - 1)]
  coordsAll  = [ Coord x y | x <- xs, y <- ys ]
  coordsFilt = filter (isStart sketch) coordsAll
  start      = head coordsFilt

getWidthHeight :: SketchOfPipes -> (Integer, Integer)
getWidthHeight sketch = (width, height)
  where
  SketchOfPipes tiles = sketch
  width               = toInteger $ length $ head tiles
  height              = toInteger $ length tiles

isStart :: SketchOfPipes -> Coord -> Bool
isStart sketch coord = is
  where
  tile = getTileAt sketch coord
  is   = tile == 'S'

getTileAt :: SketchOfPipes -> Coord -> Char
getTileAt sketch coord = tile
  where
  Coord x y           = coord
  SketchOfPipes tiles = sketch
  tile                = genericIndex (genericIndex tiles y) x

extendPath :: SketchOfPipes -> [Coord] -> [Coord]
extendPath sketch pathOld
  | (not $ elem c0 pathOld) =
    extendPath sketch $ pathOld ++ [c0]
  | (not $ elem c1 pathOld) =
    extendPath sketch $ pathOld ++ [c1]
  | (otherwise) =
    pathOld
  where
  (c0, c1) = getConnecteds sketch $ last pathOld

getConnecteds :: SketchOfPipes -> Coord -> (Coord, Coord)
--getConnecteds sketch coordOld = trace ("getConnecteds: " ++ show coordOld ++ " " ++ show coordsConnected) coordsPair
getConnecteds sketch coordOld = coordsPair
  where
  coordsNeighbors = getNeighbors sketch coordOld
  coordsConnected = filter (isAligned sketch coordOld) coordsNeighbors
  coordsPair      = (head coordsConnected, last coordsConnected)

getNeighbors :: SketchOfPipes -> Coord -> [Coord]
getNeighbors sketch coord = coordsNeighbors
  where
  Coord x y       = coord
  coordsAll       = [
    Coord (x + 1) y,
    Coord (x - 1) y,
    Coord x (y + 1),
    Coord x (y - 1)
    ]
  coordsNeighbors = filter (isWithinBounds sketch) coordsAll

isAligned :: SketchOfPipes -> Coord -> Coord -> Bool
isAligned sketch coordOrig coordOther
  | (positionOther == DirRight) =
    --trace ("isAligned: Right " ++ show coordOrig ++ " " ++ show coordOther)
    elem DirRight directionsOrig  &&
    elem DirLeft directionsOther
  | (positionOther == DirDown) =
    --trace ("isAligned: Down " ++ show coordOrig ++ " " ++ show coordOther)
    elem DirDown directionsOrig  &&
    elem DirUp directionsOther
  | (positionOther == DirLeft) =
    --trace ("isAligned: Left " ++ show coordOrig ++ " " ++ show coordOther)
    elem DirLeft directionsOrig  &&
    elem DirRight directionsOther
  | (positionOther == DirUp) =
    --trace ("isAligned: Up " ++ show coordOrig ++ " " ++ show coordOther)
    elem DirUp directionsOrig  &&
    elem DirDown directionsOther
  where
  positionOther   = getPositionRelative coordOrig coordOther
  directionsOrig  = getDirectionsOfConnections sketch coordOrig
  directionsOther = getDirectionsOfConnections sketch coordOther

isWithinBounds :: SketchOfPipes -> Coord -> Bool
isWithinBounds sketch coord = isWithin
  where
  Coord x y = coord
  (w, h)    = getWidthHeight sketch
  isWithin  = (0 <= x) && (x < w) && (0 <= y) && (y < h)

getPositionRelative :: Coord -> Coord -> Direction
getPositionRelative coordOrig coordOther
  | (xOrig < xOther) =
    DirRight
  | (xOther < xOrig) =
    DirLeft
  | (yOrig < yOther) =
    DirDown
  | (yOther < yOrig) =
    DirUp
  where
  Coord xOrig  yOrig  = coordOrig
  Coord xOther yOther = coordOther

getDirectionsOfConnections :: SketchOfPipes -> Coord -> [Direction]
getDirectionsOfConnections sketch coord
  | (tile == '|') =
    [DirUp,   DirDown]
  | (tile == '-') =
    [DirLeft, DirRight]
  | (tile == 'L') =
    [DirUp,   DirRight]
  | (tile == 'J') =
    [DirUp,   DirLeft]
  | (tile == '7') =
    [DirDown, DirLeft]
  | (tile == 'F') =
    [DirDown, DirRight]
  | (tile == '.') =
    []
  | (tile == 'S') =
    [DirUp, DirDown, DirLeft, DirRight]
  where
    tile = getTileAt sketch coord
