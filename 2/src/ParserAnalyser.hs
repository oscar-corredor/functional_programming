module ParserAnalyser
where
import System.IO  
import System.Directory
import qualified Parser
import qualified Control.Monad
import qualified GameMap

------------------------------------------------------------------
-- data Internal = Natural Int deriving Show
data Internal = Position Int Int
                | EmptyPosition deriving (Show, Eq)

data MultiInternal = Multi Internal MultiInternal 
                     | Empty deriving (Show, Eq)


data Line = Supply Int
            | ExplorerPosition Internal 
            | LineOfSight Int
            | WormLength Int
            | WormProbability Int
            | WaterCapacity Int 
            | Seed Int 
            | TreasureProb Int 
            | WaterProb Int 
            | PortalProb Int 
            | LavaProb Int 
            | AdjacentLavaProb Int 
            | Revealed Internal 
            | Collected Internal 
            | Emerging MultiInternal
            | Disappearing MultiInternal deriving Show


sameLine :: Maybe Line -> Line -> Bool
sameLine (Just (ExplorerPosition _)) (ExplorerPosition _) = True
sameLine (Just (Supply _)) (Supply _) = True
sameLine (Just (LineOfSight _)) (LineOfSight _) = True
sameLine (Just (WaterCapacity _)) (WaterCapacity _) = True
sameLine (Just (Seed _)) (Seed _) = True
sameLine (Just (TreasureProb _)) (TreasureProb _) = True
sameLine (Just (WaterProb _)) (WaterProb _) = True
sameLine (Just (PortalProb _)) (PortalProb _) = True
sameLine (Just (LavaProb _)) (LavaProb _) = True
sameLine (Just (AdjacentLavaProb _)) (AdjacentLavaProb _) = True
sameLine (Just (Revealed _)) (Revealed _) = True
sameLine (Just (Collected _)) (Collected _) = True
sameLine (Just (Emerging _)) (Emerging _) = True
sameLine (Just (Disappearing _)) (Disappearing _) = True
sameLine (Just (WormLength _)) (WormLength _) = True
sameLine (Just (WormProbability _)) (WormProbability _) = True
sameLine _ _ = False

parenthesis :: Parser.Parser a -> Parser.Parser a
parenthesis p = do Parser.keyword "("
                   x <- p
                   Parser.keyword ")"
                   return x

brackets :: Parser.Parser a -> Parser.Parser a
brackets p = do Parser.keyword "["
                x <- p                
                Parser.keyword "]"
                return x

line :: Parser.Parser Line
line = Parser.oneof[supply,explorerPosition, emerging, revealedTile, collectedTreasure, disappearing, lineOfSight, waterCapacity, seed, treasureProb, waterProb, portalProb, lavaProb, adjacentLavaProb, wormLength, wormProbability]

internals = Parser.oneof[position]

multiPos = Parser.oneof[multiPositions]

extraPos = Parser.oneof[positions,emptyPositions]

position :: Parser.Parser Internal
position = brackets $ Control.Monad.liftM2 Position Parser.fromRead (Parser.keyword "," >> Parser.fromRead)

internalPosition :: Parser.Parser Internal
internalPosition = parenthesis $ position

positions :: Parser.Parser MultiInternal
positions = Parser.keyword "," >> Control.Monad.liftM2 Multi position extraPos

emptyPositions :: Parser.Parser MultiInternal
emptyPositions = Parser.blank >> return Empty

multiPositions :: Parser.Parser MultiInternal
multiPositions = Control.Monad.liftM2 Multi position extraPos

internalMultiPositions :: Parser.Parser MultiInternal
internalMultiPositions = parenthesis $ multiPositions

explorerPosition :: Parser.Parser Line
explorerPosition =  Parser.keyword "position" >> Control.Monad.liftM ExplorerPosition internalPosition

revealedTile :: Parser.Parser Line
revealedTile =  Parser.keyword "revealed" >> Control.Monad.liftM Revealed internalPosition

collectedTreasure :: Parser.Parser Line
collectedTreasure =  Parser.keyword "collected" >> Control.Monad.liftM Collected internalPosition

supply :: Parser.Parser Line
supply = Parser.keyword "supply" >> Control.Monad.liftM Supply Parser.fromRead

lineOfSight :: Parser.Parser Line
lineOfSight = Parser.keyword "s" >> Control.Monad.liftM LineOfSight Parser.fromRead

waterCapacity :: Parser.Parser Line
waterCapacity = Parser.keyword "m" >> Control.Monad.liftM WaterCapacity Parser.fromRead

seed :: Parser.Parser Line
seed = Parser.keyword "g" >> Control.Monad.liftM Seed Parser.fromRead

treasureProb :: Parser.Parser Line
treasureProb = Parser.keyword "t" >> Control.Monad.liftM TreasureProb Parser.fromRead

waterProb :: Parser.Parser Line
waterProb = Parser.keyword "w" >> Control.Monad.liftM WaterProb Parser.fromRead

portalProb :: Parser.Parser Line
portalProb = Parser.keyword "p" >> Control.Monad.liftM PortalProb Parser.fromRead

lavaProb :: Parser.Parser Line
lavaProb = Parser.keyword "l" >> Control.Monad.liftM LavaProb Parser.fromRead

adjacentLavaProb :: Parser.Parser Line
adjacentLavaProb = Parser.keyword "ll" >> Control.Monad.liftM AdjacentLavaProb Parser.fromRead

wormLength :: Parser.Parser Line
wormLength = Parser.keyword "x" >> Control.Monad.liftM WormLength Parser.fromRead

wormProbability :: Parser.Parser Line
wormProbability = Parser.keyword "y" >> Control.Monad.liftM WormProbability Parser.fromRead

emerging :: Parser.Parser Line
emerging =  Parser.keyword "emerging" >> Control.Monad.liftM Emerging internalMultiPositions

disappearing :: Parser.Parser Line
disappearing =  Parser.keyword "disappearing" >> Control.Monad.liftM Disappearing internalMultiPositions

-- test :: [(MultiInternal, String)]
-- test = Parser.apply multiPos " [ 0 , 5 ] , [10 , 3] , [20 , 35] , [4 , 40] "
test :: [(Line, String)]
-- test = Parser.apply line "position ( [0 , 1] )"
-- test = Parser.apply line "supply ( 3 )"
-- test = Parser.apply line " supply ( 6 )"
-- test = Parser.apply line " revealed ( [0 , 1] )"
-- test = Parser.apply line "collected ( [0 , 1] )"
-- test = Parser.apply line "emerging ( [ 0 , 5 ] , [ 10 , 2 ] , [20 , 35] , [4 , 40] )"
test = Parser.apply line "disappearing ( [ 0 , 5 ] , [ 10 , 2 ] , [20 , 35] , [4 , 40] )"
-- test = Parser.apply line "s ( 10 )"
-- test = Parser.apply line "m ( 10 )"
-- test = Parser.apply line "g ( 10 )"
-- test = Parser.apply line "t ( 10 )"
-- test = Parser.apply line "w ( 10 )"
-- test = Parser.apply line "p ( 10 )"
-- test = Parser.apply line "l ( 10 )"
-- test = Parser.apply line "ll ( 10 )"
-- test = Parser.apply line "x ( 8 )"
-- test = Parser.apply line "y ( 35 )"

loadGameFromFile :: IO GameMap.GameState
loadGameFromFile =
  do
    currentDirectoryPath <- getCurrentDirectory
    let nDirectoryFilepath = (currentDirectoryPath++"/savedGames/")
    handle <- openFile (nDirectoryFilepath++"saved.txt") ReadMode  
    contents <- (hGetContents handle)
    newLines <- mapM parseLine (lines contents)    
    hClose handle    
    -- game rules initialization
    let revealedTiles = (fetchPositions newLines (Revealed (EmptyPosition)))
    let collectedTreasures = (fetchPositions newLines (Collected (EmptyPosition)))
    let (maxRow, maxColumn) = foldl (\(accRow, accCol) (row, column) -> ((max accRow row),(max accCol column))) (-1,-1) revealedTiles
    let maybeGameRules = initializeGameRules newLines
    if maybeGameRules /= Nothing
      then do        
        -- player initialization
        let (playerRow, playerColumn) = (fetchPositions newLines (ExplorerPosition (EmptyPosition)))!!0
        let (Supply waterCount) = fetchObject newLines (Supply (-1)) 
        let player = (GameMap.Player playerColumn playerRow maxRow maxColumn waterCount (length collectedTreasures))
        let (Just gameRules) = maybeGameRules
        -- board initialization
        --                                                            visitedTiles                       worms currentTurn 
        let board = GameMap.getBoard maxRow maxColumn gameRules player [] collectedTreasures revealedTiles (initializeWorms newLines) 0        
        return (GameMap.GameState board maxRow maxColumn gameRules)
      else return (GameMap.FailedGame)

parseLine :: String -> IO (Maybe Line)
parseLine str =
  do
    let parsedResults = Parser.apply line str
    if (length parsedResults) == 0 then return Nothing else let (line,emptyStr) = parsedResults!!0 in return (Just line)

fetchPositions :: [Maybe Line] -> Line -> [(Int,Int)]
fetchPositions lines positionType = map extractPosition (filter (\x -> sameLine x positionType) lines)

extractPosition :: Maybe Line -> (Int,Int)
extractPosition (Just (Revealed (Position row column))) = (row,column)
extractPosition (Just (Collected (Position row column))) = (row,column)
extractPosition (Just (ExplorerPosition (Position row column))) = (row,column)

initializeGameRules :: [Maybe Line] -> Maybe GameMap.GameRules
initializeGameRules lines = 
  let (WaterCapacity m) = fetchObject lines (WaterCapacity (-1))
      (Seed g) = fetchObject lines (Seed (-1))
      (LineOfSight s) = fetchObject lines (LineOfSight (-1))
      (TreasureProb t) = fetchObject lines (TreasureProb (-1))
      (WaterProb w) = fetchObject lines (WaterProb (-1))
      (PortalProb p) = fetchObject lines (PortalProb (-1))
      (LavaProb l) = fetchObject lines (LavaProb (-1))
      (AdjacentLavaProb ll) = fetchObject lines (AdjacentLavaProb (-1))
      (WormLength x) = fetchObject lines (WormLength (-1))
      (WormProbability y) = fetchObject lines (WormProbability (-1))
  in GameMap.initializeRules ["m", (show m), "g", (show g), "s", (show s), "t", (show t), "w", (show w), "p", (show p), "l", (show l), "ll", (show ll), "x",(show x),"y",(show y)]

fetchObject :: [Maybe Line] -> Line -> Line
fetchObject [] _ = (Seed (-1))
fetchObject (x:xs) line 
  | sameLine x line = 
      let (Just foundLine) = x
      in foundLine
  | otherwise = fetchObject xs line

initializeWorms :: [Maybe Line] -> [GameMap.Worm]
initializeWorms lines = map initializeWorm (filter (\x -> or [(sameLine x (Emerging Empty)), (sameLine x (Disappearing Empty))]) lines)

initializeWorm :: Maybe Line -> GameMap.Worm
initializeWorm (Just (Emerging positions)) = 
  let wormTiles = initializeWormTiles positions []
  in (GameMap.EmergingWorm wormTiles)
initializeWorm (Just (Disappearing positions)) = 
  let wormTiles = initializeWormTiles positions []
  in (GameMap.DissapearingWorm wormTiles)
    
-- (Emerging (Multi (Position 0 5) (Multi (Position 10 2) (Multi (Position 20 35) (Multi (Position 4 40) Empty))))
initializeWormTiles :: MultiInternal -> [(Int,Int)] -> [(Int,Int)]
initializeWormTiles (Multi (Position row column) nextPosition) tileList
  | nextPosition == Empty = tileList++[(row, column)]
  | otherwise = initializeWormTiles nextPosition tileList++[(row, column)]