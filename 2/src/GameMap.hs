module GameMap
where
import System.Random
import System.IO
import Control.Concurrent.STM
import Control.Concurrent
-- ////////////////////////////////////////////////// DATA STRUCTURES
--                              Rows Cols-> size of the map to display
type Args = [String]
data GameState = GameState Board Int Int GameRules | FailedGame
instance Eq GameState where  
  (GameState _ _ _ _) == (GameState _ _ _ _) = True  
  FailedGame == FailedGame = True    
  _ == _ = False  
type HasPlayer = Bool
type IsVisible = Bool
type HasWorm = Bool
     -- Tile Type Row Column HasPlayer IsVisible HasWorm
data Tile =  Desert Int Int HasPlayer IsVisible HasWorm| Treasure Int Int HasPlayer IsVisible HasWorm| Water Int Int HasPlayer IsVisible HasWorm| Lava Int Int HasPlayer IsVisible HasWorm| Portal Int Int HasPlayer IsVisible HasWorm| None Int Int HasPlayer IsVisible HasWorm deriving (Eq)
instance Show Tile where  
  show (Desert row column False True False)= " . "
  show (Treasure row column False True False)= " T "  
  show (Water row column False True  False)= " ~ "
  show (Lava row column False True  False)= " * "
  show (Portal row column False True  False)= " @ "
  show (None row column False True False)= " % "
  
  show (Desert row column False True True)= " < "
  show (Treasure row column False True True)= " < "  
  show (Water row column False True  True)= " < "
  show (Lava row column False True  True)= " < "
  show (Portal row column False True  True)= " < "
  show (None row column False True True)= " < "

  show (Desert row column True True  False)= "[P]"
  show (Treasure row column True True  False)= "[P]"  
  show (Water row column True True False)= "[P]"
  show (Lava row column True True False)= "[P]"
  show (Portal row column True True  False)= "[P]"
  show (None row column True True False)= "[P]"
  show _ = "   "

-- srry about this, I know it can be done easier with record syntax...
getColumnCoordinate :: Tile -> Int
getColumnCoordinate (Desert _ x _ _ _)   = x
getColumnCoordinate (Treasure _ x _ _ _) = x
getColumnCoordinate (Water _ x _ _ _)    = x
getColumnCoordinate (Lava _ x _ _ _)     = x
getColumnCoordinate (Portal _ x _ _ _)   = x
getColumnCoordinate (None _ x _ _ _)     = x

getRowCoordinate :: Tile -> Int
getRowCoordinate (Desert row _ _ _ _)   = row
getRowCoordinate (Treasure row _ _ _ _) = row
getRowCoordinate (Water row _ _ _ _)    = row
getRowCoordinate (Lava row _ _ _ _)     = row
getRowCoordinate (Portal row _ _ _ _)   = row
getRowCoordinate (None row _ _ _ _)     = row

getContainsPlayer :: Tile -> Bool
getContainsPlayer (Desert _ _ hasPlayer _ _)   = hasPlayer
getContainsPlayer (Treasure _ _ hasPlayer _ _) = hasPlayer
getContainsPlayer (Water _ _ hasPlayer _ _)    = hasPlayer
getContainsPlayer (Lava _ _ hasPlayer _ _)     = hasPlayer
getContainsPlayer (Portal _ _ hasPlayer _ _)   = hasPlayer
getContainsPlayer (None _ _ hasPlayer _ _)     = hasPlayer

getHasWorm :: Tile -> Bool
getHasWorm (Desert _ _ _ _ hasWorm)   = hasWorm
getHasWorm (Treasure _ _ _ _ hasWorm) = hasWorm
getHasWorm (Water _ _ _ _ hasWorm)    = hasWorm
getHasWorm (Lava _ _ _ _ hasWorm)     = hasWorm
getHasWorm (Portal _ _ _ _ hasWorm)   = hasWorm
getHasWorm (None _ _ _ _ hasWorm)     = hasWorm

sameKind :: Tile -> Tile -> Bool
sameKind (Desert _ _ _ _ _) (Desert _ _ _ _ _) = True
sameKind (Treasure _ _ _ _ _) (Treasure _ _ _ _ _) = True
sameKind (Water _ _ _ _ _) (Water _ _ _ _ _) = True
sameKind (Lava _ _ _ _ _) (Lava _ _ _ _ _) = True
sameKind (Portal _ _ _ _ _) (Portal _ _ _ _ _) = True
sameKind (None _ _ _ _ _) (None _ _ _ _ _) = True
sameKind _ _ = False

data Player = Player { x :: Int  
  , y :: Int
  , maxX :: Int
  , maxY :: Int
, water :: Int
, treasureCount :: Int
} deriving (Show)   

data GameRules = GameRules { normalProbabilities :: [(Tile,Float)]
, lavaProbabilities :: [(Tile,Float)] 
, canteenCapacity :: Int 
, seed :: Int
, lineOfSight :: Int
, args :: [String]
, wormLength :: Int
, wormProbability :: Float} deriving (Show, Eq)

executionParams = ["m","g","t","w","p","l","ll"]
initializeRules :: [String] -> Maybe GameRules
initializeRules args
  | length args /= 20 || length (filter (\x -> (not (elem x args))) executionParams) > 0 = Nothing
  | otherwise = 
    let s  = (fetchValue "s" args)
        m  = fetchValue "m" args
        g  = fetchValue "g" args
        -- probabilities
        t  = (fetchValue "t" args)/100  
        w  = (fetchValue "w" args)/100  
        p  = (fetchValue "p" args)/100  
        l  = (fetchValue "l" args)/100  
        ll = (fetchValue "ll" args)/100 
        -- worm parameters
        x = (fetchValue "x" args)
        y = (fetchValue "y" args)/100
        -- normal case
        normalDessertGeneralP = 1 - (w+p+l)
        normalTreasureP = normalDessertGeneralP*t
        normalDessertTileP = normalDessertGeneralP - normalTreasureP
        -- lava case
        lavaDessertGeneralP = 1 - (w+p+ll)
        lavaTreasureP = lavaDessertGeneralP*t
        lavaDessertTileP = lavaDessertGeneralP - lavaTreasureP        
    in if (w + p + l > 1) || (w + p + ll > 1) || (s <= 0) || (m <= 0) || (g <= 0) || (t <= 0) || (l <= 0) || (ll <= 0) || (x <= 0) || (y <= 0)   then Nothing else Just (GameRules [((Treasure 1 1 False False False),normalTreasureP) , ((Water 1 1 False False False),w) , ((Portal 1 1 False False False), p) , ((Desert 1 1 False False False), normalDessertTileP), ((Lava 1 1 False False False), l)] [((Treasure 1 1 False False False),lavaTreasureP), ((Water 1 1 False False False),w), ((Portal 1 1 False False False), p), ((Desert 1 1 False False False), lavaDessertTileP), ((Lava 1 1 False False False), ll)] (round m) (round g) (round s) args (round x) y)



fetchValue :: String -> [String] -> Float
fecthV  _ [] = 0.0
fetchValue name (p:v:rest) -- some error handling should go here
  | name == p = (read v :: Float)
  | otherwise = fetchValue name rest


data GameStatus = Normal | Lost | Won deriving Show

type VisitedTiles = [(Int,Int)]
type CollectedTreasures = [(Int,Int)]
type RevealedTiles = [(Int,Int)]
type CurrentTurn = Int
                  --map      player  status  visitedTiles                      infiniteTiles (DESERT) worms
data Board = Board [[Tile]] Player GameStatus VisitedTiles CollectedTreasures RevealedTiles [[Tile]] [Worm] CurrentTurn deriving Show

data Worm = EmergingWorm [(Int,Int)] | DissapearingWorm [(Int,Int)] deriving Show

isEmergingWorm :: Worm -> Bool
isEmergingWorm (EmergingWorm _) = True
isEmergingWorm _ = False

-- ///////////////////////////////////////////////////////// FUNCTIONS

desert :: GameRules -> Player -> VisitedTiles -> CollectedTreasures -> RevealedTiles -> [Worm] -> [[Tile]]
desert gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args wormLength wormProbability) player visitedTiles collectedTreasures revealedTiles worms= 
  [zipWith (\column randomNumber -> (nGenerateTile (row,(column, randomNumber)) gameRules player visitedTiles collectedTreasures revealedTiles worms)) [0..] (randomRs (0,1) (mkStdGen (seed + row)) :: [Float]) | row <- [0..]]
-- map (take 10) (take 1 (indexedMatrix 1))
-- map (take 10) (take 1 (desert 1 [((Treasure 1 1),0.18), ((Water  1 1),0.15), ((Portal  1 1),0.15), ((Lava  1 1), 0.1), ((Desert  1 1), 0.6)] [((Treasure 1 1),0.18), ((Water  1 1),0.15), ((Portal  1 1),0.15), ((Lava  1 1), 0.1), ((Desert  1 1), 0.6)]))

getBoard :: Int -> Int -> GameRules -> Player -> VisitedTiles-> CollectedTreasures -> RevealedTiles -> [Worm] -> CurrentTurn -> Board
getBoard rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args wormLength wormProbability) player@(Player x y _ _ _ _) visitedTiles collectedTreasures revealedTiles worms currentTurn =
  let infiniteTiles = (desert gameRules player visitedTiles collectedTreasures revealedTiles worms)
      tiles = map (take columns) (take rows infiniteTiles)
      nWorms = spawnWorms tiles worms currentTurn wormProbability
      (nPlayer, nCollectedTreasures) = updatePlayer player (tiles!!y!!x) gameRules visitedTiles collectedTreasures
      gameStatus = updateGameStatus nPlayer (tiles!!y!!x)
      nVisitedTiles = ((y,x):visitedTiles)  
  in (Board tiles nPlayer gameStatus nVisitedTiles nCollectedTreasures revealedTiles infiniteTiles (worms++nWorms) currentTurn)

updateGameStatus :: Player -> Tile -> GameStatus
updateGameStatus _ (Desert _ _ _ _ True) = Lost
updateGameStatus _ (Lava _ _ _ _ _) = Lost
updateGameStatus _ (Portal _ _ _ _ _) = Won
updateGameStatus (Player _ _ _ _ water _) _
  | water <= 0 = Lost
  | otherwise = Normal

nGenerateTile :: (Int, (Int, Float)) -> GameRules -> Player -> VisitedTiles -> CollectedTreasures -> RevealedTiles -> [Worm] -> Tile
nGenerateTile (rowIndex, (columnIndex, value)) gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args wormLength wormProbability) player visitedTiles collectedTreasures  revealedTiles worms
  | (nLavaInPreviousTiles columnIndex rowIndex normalProbabilities lavaProbabilities seed)  = computeTileValue (rowIndex, (columnIndex, value)) lavaProbabilities player lineOfSight visitedTiles collectedTreasures revealedTiles worms
  | otherwise = computeTileValue (rowIndex, (columnIndex, value)) normalProbabilities player lineOfSight visitedTiles collectedTreasures revealedTiles worms


-- nLavaInPreviousTiles :: Int -> Int -> Bool
nLavaInPreviousTiles row column normalProbabilities lavaProbabilities seed = False
  -- | sameKind (fetchTile row (column-1) (desert seed normalProbabilities lavaProbabilities)) (Lava (-1) (-1)) || sameKind (fetchTile (row-1) (column) (desert seed normalProbabilities lavaProbabilities)) (Lava (-1) (-1)) = True
  -- | otherwise = False
  
fetchTile :: Int -> Int -> [[Tile]] -> Tile
fetchTile row column desert
  | row < 0 || column < 0 = (None (-1) (-1) False False False)
  | otherwise = (desert!!row)!!column

lavaInPreviousTiles :: [Tile] -> Tile -> Int -> Bool
lavaInPreviousTiles previousRow previousTile columnIndex
  | previousRow == [] = if (sameKind (Lava (-1) (-1) True True True) previousTile) then True else False
  -- | columnIndex == 0 = if previousTile == Lava || previousRow !! (columnIndex+1) == Lava || previousRow !! (columnIndex) == Lava then True else False
  -- | previousTile == Lava || previousRow !! (columnIndex+1) == Lava || previousRow !! (columnIndex) == Lava || previousRow !! (columnIndex-1) == Lava = True
  | sameKind (Lava (-1) (-1) False False False) previousTile|| sameKind (Lava (-1) (-1) False False False) (previousRow!!columnIndex) = True
  | otherwise = False

tileIsLava :: Tile -> Bool
tileIsLava (Lava _ _ _ _ _) = True
tileIsLava _ = False

computeTileValue :: (Int, (Int, Float)) -> [(Tile,Float)] -> Player -> Int -> VisitedTiles -> CollectedTreasures -> RevealedTiles -> [Worm] -> Tile
computeTileValue (rowIndex, (columnIndex, value)) [] _  _ _ _ _ _= (None columnIndex rowIndex False False False) --If we reach this case, we done fucked up.
computeTileValue (rowIndex, (columnIndex, value)) ((c,p):ps) player@(Player playerX playerY maxX maxY _ _) lineOfSight visitedTiles collectedTreasures revealedTiles worms -- where c is the tile value and p is the value of the probability
  | value < p =    
    let isVisible = elem (rowIndex,columnIndex) revealedTiles
        hasWorm = any (\x -> wormContainsTile (rowIndex,columnIndex) x) worms
        hasPlayer = (rowIndex == playerY && columnIndex == playerX)        
    in (getTileValue c columnIndex rowIndex hasPlayer isVisible hasWorm visitedTiles collectedTreasures) 
  | otherwise = computeTileValue (rowIndex, (columnIndex, value-p)) ps player lineOfSight visitedTiles collectedTreasures revealedTiles worms

wormContainsTile :: (Int,Int) -> Worm -> Bool
wormContainsTile tile (EmergingWorm wormTiles) = elem tile wormTiles
wormContainsTile tile (DissapearingWorm wormTiles) = elem tile wormTiles

withinReach :: [(Int,Int)] -> Int -> Int -> Int -> Bool
withinReach visitedTiles row column lineOfSight = any (\(visitedRow, visitedColumn) -> ((abs (row - visitedRow))  <= lineOfSight) && ((abs (column - visitedColumn))  <= lineOfSight) ) visitedTiles


withinRange :: Int -> Int -> Int -> Int -> Int -> Bool
withinRange playerXPos playerYPos row column range = ((abs playerXPos-column) <= range) && ((abs playerYPos-row) <= range)
 
distanceFormula :: (Int,Int) -> (Int,Int) -> Float
distanceFormula (x1,y1) (x2,y2) = 
  let x =((x2-x1)^2 )
      y = ((y2-y1)^2)
  in (sqrt (fromIntegral x)+(fromIntegral y))

type IsWithinRange = Bool

getTileValue :: Tile -> Int -> Int -> HasPlayer -> IsVisible -> HasWorm -> VisitedTiles -> CollectedTreasures -> Tile
-- tile 0,0 always should be desert
getTileValue _ 0 0 hasPlayer isVisible _ _ _ = (Desert 0 0 hasPlayer isVisible False)
getTileValue (Desert _ _ _ _ _) x y hasPlayer isVisible hasWorm _ _= (Desert y x hasPlayer isVisible hasWorm)
getTileValue (Treasure _ _ _ _ _) x y hasPlayer isVisible _ visitedTiles collectedTreasures
  -- in case the dessert tile has already been visited, replace it by a desert one
  | (elem (y,x) collectedTreasures) = (Desert y x hasPlayer isVisible False)
  | otherwise = (Treasure y x hasPlayer isVisible False)
getTileValue (Water _ _ _ _ _) x y hasPlayer isVisible _ _ _= (Water y x hasPlayer isVisible False)
getTileValue (Lava _ _ _ _ _) x y hasPlayer isVisible _ _ _= (Lava y x hasPlayer isVisible False)
getTileValue (Portal _ _ _ _ _) x y hasPlayer isVisible _ _ _= (Portal y x hasPlayer isVisible False)
getTileValue _ x y hasPlayer isVisible _ _ _= (None x y hasPlayer isVisible False)

updatePlayer :: Player -> Tile -> GameRules -> VisitedTiles -> CollectedTreasures -> (Player,CollectedTreasures)
-- the player should not lose water at spawn
updatePlayer player@(Player 0 0 _ _ water treasureCount) (Desert _ _ _ _ _) _ _ collectedTreasures= (player,collectedTreasures)
updatePlayer (Player x y maxX maxY water treasureCount) (Desert _ _ _ _ _) _ _ collectedTreasures= ((Player x y (max maxX x) (max maxY y) (water-1) treasureCount),collectedTreasures)
updatePlayer (Player x y maxX maxY water treasureCount) (Treasure row column _ _ _) _ visitedTiles collectedTreasures
 |(not (elem (row,column) collectedTreasures)) = ((Player x y (max maxX x) (max maxY y) (water-1) (treasureCount+1)), (collectedTreasures++[(row,column)]))
 | otherwise = ((Player x y (max maxX x) (max maxY y) (water-1) (treasureCount)),collectedTreasures)
updatePlayer (Player x y maxX maxY water treasureCount) (Water _ _ _ _ _) (GameRules _ _ canteenCapacity _ _ _ _ _) _ collectedTreasures= ((Player x y (max maxX x) (max maxY y) canteenCapacity treasureCount), collectedTreasures)
updatePlayer (Player x y maxX maxY water treasureCount) (Lava _ _ _ _ _) _ _ collectedTreasures= ((Player x y (max maxX x) (max maxY y) (water-1) treasureCount),collectedTreasures)
updatePlayer (Player x y maxX maxY water treasureCount) (Portal _ _ _ _ _) _ _ collectedTreasures= ((Player x y (max maxX x) (max maxY y) (water-1) treasureCount), collectedTreasures)
updatePlayer (Player x y maxX maxY water treasureCount) _ _ _ collectedTreasures = ((Player x y (max maxX x) (max maxY y) (water-1) treasureCount), collectedTreasures)

useMagicCompass :: [[Tile]] -> Tile -> Int -> Int -> Int
useMagicCompass tiles objectiveTile rowIndex columnIndex = 
  let neighBoringTiles = getNeighboringTiles columnIndex rowIndex [] tiles 
  in findClosest neighBoringTiles [(tiles!!rowIndex!!columnIndex)] tiles objectiveTile 1

findClosest :: [Tile] -> [Tile] -> [[Tile]] -> Tile -> Int -> Int
findClosest neighBoringTiles visitedTiles tiles objectiveTile acc
  -- case where we found the tile we're looking for
  | (length (filter (sameKind objectiveTile) neighBoringTiles)) > 0 = acc
  -- havent found it yet, need to keep looking, add the next neighboring tiles that have not been visited yet
  | otherwise =
      let nVisitedTiles     = visitedTiles++neighBoringTiles          
          nNeighboringTiles = computeNNeighboringTiles neighBoringTiles nVisitedTiles tiles []      
      in findClosest nNeighboringTiles nVisitedTiles tiles objectiveTile (acc+1)


computeNNeighboringTiles :: [Tile] -> [Tile] -> [[Tile]] -> [Tile] -> [Tile]
computeNNeighboringTiles [] visitedTiles tiles acc     = acc
computeNNeighboringTiles (t:ts) visitedTiles tiles acc =
  let tileNeighbors = (getNeighboringTiles (getColumnCoordinate t) (getRowCoordinate t) visitedTiles tiles)  
  in computeNNeighboringTiles ts (visitedTiles++tileNeighbors) tiles (acc++tileNeighbors)


  -- gets the non visited neighboring tiles
getNeighboringTiles :: Int -> Int -> [Tile] -> [[Tile]] -> [Tile]
getNeighboringTiles columnIndex rowIndex visitedTiles tiles =
  let upperTile = fetchTile (rowIndex-1) columnIndex tiles
      lowerTile = fetchTile (rowIndex+1) columnIndex tiles
      leftTile  = fetchTile (rowIndex) (columnIndex-1) tiles
      rightTile = fetchTile (rowIndex) (columnIndex+1) tiles
  in (filter (\x -> (not (sameKind (None (-1) (-1) False False False) x) ) && (not (sameKind (Lava (-1) (-1) False False False) x)) && (not (elem x visitedTiles))) [upperTile,rightTile,lowerTile,leftTile])

-- WORMS LOGIC -----------------------------------
-- WORM SPAWNING
spawnWorms :: [[Tile]] -> [Worm] -> CurrentTurn -> Float -> [Worm]
spawnWorms tiles currentWorms currentTurn wormProbability =
  let spawnableTiles = filterSpawnableTiles tiles currentWorms currentTurn wormProbability
  in map (\tile -> (EmergingWorm [((getRowCoordinate tile),(getColumnCoordinate tile))])) spawnableTiles

filterSpawnableTiles :: [[Tile]] -> [Worm] -> CurrentTurn -> Float -> [Tile]
filterSpawnableTiles tiles currentWorms currentTurn wormProbability = foldl (\acc x -> x++acc) [] (map (filter (\tile -> (canSpawnWorm tile currentTurn currentWorms wormProbability))) tiles)

canSpawnWorm :: Tile -> CurrentTurn -> [Worm] -> Float -> Bool
canSpawnWorm tile currentTurn currentWorms wormProbability 
  | (sameKind (Desert 0 0 True True True) tile) && (not (getHasWorm tile)) && (not (getContainsPlayer tile)) = rollWorms ((getRowCoordinate tile),(getColumnCoordinate tile)) currentTurn wormProbability
  | otherwise = False

rollWorms :: (Int,Int) -> Int -> Float -> Bool
rollWorms (rowIndex, columnIndex) currentTurn wormProbability =
  let [randomNumber] = (take 1 (randomRs (0,1) (mkStdGen (currentTurn * rowIndex * columnIndex)) :: [Float]))
  in randomNumber <= wormProbability
---------------
-- WORM SIMULATION
-- simulateWorms :: [[Tile]] -> [Worm] -> [Worm]
-- simulateWorms tiles currentWorms = filter (\worm -> wormIsAlive worm) (map (\worm -> (simulateWorm tiles currentWorms worm)) currentWorms)

wormIsAlive :: Worm -> Bool
wormIsAlive (DissapearingWorm []) = False
wormIsAlive (EmergingWorm []) = False -- in theory this case should never occur... but you never know.
wormIsAlive _ = True

simulateWorm :: [[Tile]] -> [Worm] -> Worm -> Int -> Float -> Worm
simulateWorm tiles currentWorms worm@(EmergingWorm wormTiles@(h:t)) wormLength wormProbability =
  let (canMove, nextWormMove) = getNextEmergingWormMove tiles currentWorms h
  in if (canMove && ((length wormTiles)<wormLength)) then (EmergingWorm ([nextWormMove]++wormTiles)) else simulateWorm tiles currentWorms (DissapearingWorm wormTiles) wormLength wormProbability
--simulateWorm tiles currentWorms worm@(DissapearingWorm []) = (DissapearingWorm [])
simulateWorm tiles currentWorms worm@(DissapearingWorm wormTiles) wormLength wormProbability = (DissapearingWorm (init wormTiles))


getNextEmergingWormMove :: [[Tile]] -> [Worm] -> (Int,Int) -> (Bool,(Int,Int))
getNextEmergingWormMove tiles currentWorms (rowIndex,columnIndex) = 
  -- without treasure and without a worms body
  let upperTile = (fetchTile (rowIndex-1) columnIndex tiles)
      canMoveUp = (sameKind (Desert (-1) (-1) True True True) upperTile) && (not (getHasWorm upperTile)) 
      lowerTile = (fetchTile (rowIndex+1) columnIndex tiles)
      canMoveDown = (sameKind (Desert (-1) (-1) True True True) lowerTile) && (not (getHasWorm lowerTile))
      rightTile = (fetchTile (rowIndex) (columnIndex+1) tiles)
      canMoveRight = (sameKind (Desert (-1) (-1) True True True) rightTile) && (not (getHasWorm rightTile))
      leftTile = (fetchTile (rowIndex) (columnIndex-1) tiles)
      canMoveLeft = (sameKind (Desert (-1) (-1) True True True) leftTile) && (not (getHasWorm leftTile))
  in determineMove [(canMoveUp, ((getRowCoordinate upperTile),(getColumnCoordinate upperTile))), (canMoveDown,((getRowCoordinate lowerTile), (getColumnCoordinate lowerTile))), (canMoveRight, ((getRowCoordinate rightTile),(getColumnCoordinate rightTile))),(canMoveLeft,((getRowCoordinate leftTile),(getColumnCoordinate leftTile)))]

determineMove :: [(Bool,(Int,Int))] -> (Bool,(Int,Int))
determineMove [] = (False,(-1,-1))
determineMove ((canMove,position):xs)
  | canMove == True = (True,position)
  | otherwise = determineMove xs

-- CONCURRENT SIMULATION
filterConcurrentWorms :: IO [Worm] -> IO [Worm]
filterConcurrentWorms ioWorms =
  do 
    worms <- ioWorms
    let filteredWorms = filter wormIsAlive worms
    return filteredWorms


awaitConcurrentWorms :: [IO (TMVar Worm)] -> IO [Worm]
awaitConcurrentWorms tMVars = mapM fetchWormFromTMVar tMVars

simulateWormsConcurrent ::[[Tile]] -> [Worm] -> Int -> Float -> [IO (TMVar Worm)] 
simulateWormsConcurrent tiles currentWorms wormLength wormProbability = map (\worm -> simulateWormConcurrent tiles currentWorms worm wormLength wormProbability) currentWorms

fetchWormFromTMVar :: IO (TMVar Worm) -> IO Worm
fetchWormFromTMVar ioTMVar = 
  do 
    tMvar <- ioTMVar
    wormValue <- atomically (takeTMVar tMvar)
    return wormValue



simulateWormConcurrent :: [[Tile]] -> [Worm] -> Worm -> Int -> Float -> IO (TMVar Worm)
simulateWormConcurrent tiles currentWorms worm wormLength wormProbability=
  do
    nWormTMVar <- atomically (newEmptyTMVar)    
    forkIO(spawnNewWorm nWormTMVar tiles currentWorms worm wormLength wormProbability)
    return nWormTMVar
    

spawnNewWorm :: TMVar Worm -> [[Tile]] -> [Worm] -> Worm -> Int -> Float -> IO ()
spawnNewWorm wormTMVar tiles currentWorms worm wormLength wormProbability =
  do
    atomically(do putTMVar wormTMVar (simulateWorm tiles currentWorms worm wormLength wormProbability)) 