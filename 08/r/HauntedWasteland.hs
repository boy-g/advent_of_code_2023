module Main (main) where

data PouchDuMaps =
  PouchDuMaps {
    pouchDuMapsInstrs :: [Instr],
    pouchDuMapsNodes  :: [Node]
  }
  deriving (Show)

data Instr =
  InstrLeft |
  InstrRight
  deriving (Show)

data Node =
  Node {
    nodeLabel :: String,
    nodeLeft  :: String,
    nodeRight :: String
  }
  deriving (Show)

main :: IO ()
main = do
  puzzleInput <- getContents
  print $ parsePuzzleInput puzzleInput

parsePuzzleInput :: String -> PouchDuMaps
parsePuzzleInput puzzleInput = pouchDuMaps
  where
  pouchDuMaps  = PouchDuMaps instructions nodes
  instructions = parseInstrs puzzleInput
  nodes        = parseNodes puzzleInput

parseInstrs :: String -> [Instr]
parseInstrs puzzleInput = instructions
  where
  instructions = map parseInstr line0
  line0        = head $ lines puzzleInput

parseInstr :: Char -> Instr
parseInstr 'L' = InstrLeft
parseInstr 'R' = InstrRight
parseInstr _   = error "parseInstr: wtf"

parseNodes :: String -> [Node]
parseNodes puzzleInput = nodes
  where
  nodes        = map parseNode linesDuNodes
  linesDuNodes = drop 2 $ lines puzzleInput

parseNode :: String -> Node
parseNode lineDuNode = node
  where
  node         = Node theNodeLabel theNodeLeft theNodeRight
  theNodeLabel = parseNodeLabel lineDuNode
  theNodeLeft  = parseNodeLeft lineDuNode
  theNodeRight = parseNodeRight lineDuNode

parseNodeLabel :: String -> String
parseNodeLabel lineDuNode = theNodeLabel
  where
  theNodeLabel = head $ words lineDuNode

parseNodeLeft :: String -> String
parseNodeLeft lineDuNode = theNodeLeft
  where
  theNodeLeftWithCruft = words lineDuNode !! 2
  theNodeLeft          = init $ drop 1 theNodeLeftWithCruft

parseNodeRight :: String -> String
parseNodeRight lineDuNode = theNodeRight
  where
  theNodeRightWithCruft = last $ words lineDuNode
  theNodeRight          = init theNodeRightWithCruft
