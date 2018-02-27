module GameMap
where
import System.Random

-- ////////////////////////////////////////////////// DATA STRUCTURES
-- Tile Type Row Column HasPlayer IsVisible
data Tile =  Desert Int Int Bool Bool| Treasure Int Int Bool Bool| Water Int Int Bool Bool| Lava Int Int Bool Bool| Portal Int Int Bool Bool| None Int Int Bool Bool deriving (Eq)
instance Show Tile where  
  show (Desert row column False True)= " . "
  show (Treasure row column False True)= " T "  
  show (Water row column False True)= " ~ "
  show (Lava row column False True)= " * "
  show (Portal row column False True)= " @ "
  show (None row column False True)= " % "
  
  show (Desert row column True True)= "[P]"
  show (Treasure row column True True)= "[P]"  
  show (Water row column True True)= "[P]"
  show (Lava row column True True)= "[P]"
  show (Portal row column True True)= "[P]"
  show (None row column True True)= "[P]"
  show _ = "   "

-- srry about this, I know it can be done easier with record syntax...
getColumnCoordinate :: Tile -> Int
getColumnCoordinate (Desert _ x _ _)   = x
getColumnCoordinate (Treasure _ x _ _) = x
getColumnCoordinate (Water _ x _ _)    = x
getColumnCoordinate (Lava _ x _ _)     = x
getColumnCoordinate (Portal _ x _ _)   = x
getColumnCoordinate (None _ x _ _)     = x

getRowCoordinate :: Tile -> Int
getRowCoordinate (Desert row _ _ _)   = row
getRowCoordinate (Treasure row _ _ _) = row
getRowCoordinate (Water row _ _ _)    = row
getRowCoordinate (Lava row _ _ _)     = row
getRowCoordinate (Portal row _ _ _)   = row
getRowCoordinate (None row _ _ _)     = row



sameKind :: Tile -> Tile -> Bool
sameKind (Desert _ _ _ _) (Desert _ _ _ _) = True
sameKind (Treasure _ _ _ _) (Treasure _ _ _ _) = True
sameKind (Water _ _ _ _) (Water _ _ _ _) = True
sameKind (Lava _ _ _ _) (Lava _ _ _ _) = True
sameKind (Portal _ _ _ _) (Portal _ _ _ _) = True
sameKind (None _ _ _ _) (None _ _ _ _) = True
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
, lineOfSight :: Int} deriving (Show, Eq)

data GameStatus = Normal | Lost | Won deriving Show

data Board = Board [[Tile]] Player GameStatus [(Int,Int)] [[Tile]] deriving Show


-- ///////////////////////////////////////////////////////// FUNCTIONS

desert :: GameRules -> Player -> [(Int,Int)] -> [[Tile]]
desert gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight) player visitedTiles= 
  [zipWith (\column randomNumber -> (nGenerateTile (row,(column, randomNumber)) gameRules player visitedTiles)) [0..] (randomRs (0,1) (mkStdGen (seed + row)) :: [Float]) | row <- [0..]]
-- map (take 10) (take 1 (indexedMatrix 1))
-- map (take 10) (take 1 (desert 1 [((Treasure 1 1),0.18), ((Water  1 1),0.15), ((Portal  1 1),0.15), ((Lava  1 1), 0.1), ((Desert  1 1), 0.6)] [((Treasure 1 1),0.18), ((Water  1 1),0.15), ((Portal  1 1),0.15), ((Lava  1 1), 0.1), ((Desert  1 1), 0.6)]))

getBoard :: Int -> Int -> GameRules -> Player -> [(Int,Int)] -> Board
getBoard rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight) player@(Player x y _ _ _ _) visitedTiles =
  let infiniteTiles = (desert gameRules player visitedTiles)
      tiles = map (take columns) (take rows infiniteTiles)
      nPlayer = updatePlayer player (tiles!!y!!x) gameRules visitedTiles
      gameStatus = updateGameStatus nPlayer (tiles!!y!!x)
      nVisitedTiles = ((y,x):visitedTiles)
  in (Board tiles nPlayer gameStatus nVisitedTiles infiniteTiles)

updateGameStatus :: Player -> Tile -> GameStatus
updateGameStatus _ (Lava _ _ _ _) = Lost
updateGameStatus _ (Portal _ _ _ _) = Won
updateGameStatus (Player _ _ _ _ water _) _
  | water <= 0 = Lost
  | otherwise = Normal

nGenerateTile :: (Int, (Int, Float)) -> GameRules -> Player -> [(Int,Int)] -> Tile
nGenerateTile (rowIndex, (columnIndex, value)) gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight) player visitedTiles    
  | (nLavaInPreviousTiles columnIndex rowIndex normalProbabilities lavaProbabilities seed)  = computeTileValue (rowIndex, (columnIndex, value)) lavaProbabilities player lineOfSight visitedTiles
  | otherwise = computeTileValue (rowIndex, (columnIndex, value)) normalProbabilities player lineOfSight visitedTiles


-- nLavaInPreviousTiles :: Int -> Int -> Bool
nLavaInPreviousTiles row column normalProbabilities lavaProbabilities seed = False
  -- | sameKind (fetchTile row (column-1) (desert seed normalProbabilities lavaProbabilities)) (Lava (-1) (-1)) || sameKind (fetchTile (row-1) (column) (desert seed normalProbabilities lavaProbabilities)) (Lava (-1) (-1)) = True
  -- | otherwise = False
  
fetchTile :: Int -> Int -> [[Tile]] -> Tile
fetchTile row column desert
  | row < 0 || column < 0 = (None (-1) (-1) False False)
  | otherwise = (desert!!row)!!column

lavaInPreviousTiles :: [Tile] -> Tile -> Int -> Bool
lavaInPreviousTiles previousRow previousTile columnIndex
  | previousRow == [] = if (sameKind (Lava (-1) (-1) True True) previousTile) then True else False
  -- | columnIndex == 0 = if previousTile == Lava || previousRow !! (columnIndex+1) == Lava || previousRow !! (columnIndex) == Lava then True else False
  -- | previousTile == Lava || previousRow !! (columnIndex+1) == Lava || previousRow !! (columnIndex) == Lava || previousRow !! (columnIndex-1) == Lava = True
  | sameKind (Lava (-1) (-1) False False) previousTile|| sameKind (Lava (-1) (-1) False False) (previousRow!!columnIndex) = True
  | otherwise = False

tileIsLava :: Tile -> Bool
tileIsLava (Lava _ _ _ _) = True
tileIsLava _ = False

computeTileValue :: (Int, (Int, Float)) -> [(Tile,Float)] -> Player -> Int -> [(Int,Int)] -> Tile
computeTileValue (rowIndex, (columnIndex, value)) [] _  _ _= (None columnIndex rowIndex False False) --If we reach this case, we done fucked up.
computeTileValue (rowIndex, (columnIndex, value)) ((c,p):ps) player@(Player playerX playerY maxX maxY _ _) lineOfSight visitedTiles -- where c is the tile value and p is the value of the probability
  | value < p =
    -- let isVisible = withinReach visitedTiles rowIndex columnIndex lineOfSight
    let isVisible = withinReach visitedTiles rowIndex columnIndex lineOfSight
    in if (rowIndex == playerY && columnIndex == playerX) then (getTileValue c columnIndex rowIndex True isVisible visitedTiles) else (getTileValue c columnIndex rowIndex False isVisible visitedTiles)
  | otherwise = computeTileValue (rowIndex, (columnIndex, value-p)) ps player lineOfSight visitedTiles

withinReach :: [(Int,Int)] -> Int -> Int -> Int -> Bool
withinReach visitedTiles row column lineOfSight = any (\(visitedRow, visitedColumn) -> ((abs (row - visitedRow))  <= lineOfSight) && ((abs (column - visitedColumn))  <= lineOfSight) ) visitedTiles
-- withinReach visitedTiles row column lineOfSight = any (\(visitedRow, visitedColumn) -> ((distanceFormula (column, row) (visitedColumn, visitedRow))  <= (fromIntegral lineOfSight))) visitedTiles

distanceFormula :: (Int,Int) -> (Int,Int) -> Float
distanceFormula (x1,y1) (x2,y2) = 
  let x =((x2-x1)^2 )
      y = ((y2-y1)^2)
  in (sqrt (fromIntegral x)+(fromIntegral y))

getTileValue :: Tile -> Int -> Int -> Bool -> Bool -> [(Int,Int)] -> Tile
-- tile 0,0 always should be desert
getTileValue _ 0 0 hasPlayer isVisible _ = (Desert 0 0 hasPlayer isVisible)
getTileValue (Desert _ _ _ _) x y hasPlayer isVisible _ = (Desert y x hasPlayer isVisible)
getTileValue (Treasure _ _ _ _) x y hasPlayer isVisible visitedTiles
  -- in case the dessert tile has already been visited, replace it by a desert one
  | (elem (y,x) visitedTiles) = (Desert y x hasPlayer isVisible)
  | otherwise = (Treasure y x hasPlayer isVisible)
getTileValue (Water _ _ _ _) x y hasPlayer isVisible _ = (Water y x hasPlayer isVisible)
getTileValue (Lava _ _ _ _) x y hasPlayer isVisible _ = (Lava y x hasPlayer isVisible)
getTileValue (Portal _ _ _ _) x y hasPlayer isVisible _ = (Portal y x hasPlayer isVisible)
getTileValue _ x y hasPlayer isVisible _ = (None x y hasPlayer isVisible)

updatePlayer :: Player -> Tile -> GameRules -> [(Int,Int)] -> Player
-- the player should not lose water at spawn
updatePlayer player@(Player 0 0 _ _ water treasureCount) (Desert _ _ _ _) _ _= player
updatePlayer (Player x y maxX maxY water treasureCount) (Desert _ _ _ _) _ _= (Player x y (max maxX x) (max maxY y) (water-1) treasureCount)
updatePlayer (Player x y maxX maxY water treasureCount) (Treasure row column _ _) _ visitedTiles
 |(not (elem (row,column) visitedTiles)) = (Player x y (max maxX x) (max maxY y) (water-1) (treasureCount+1))
 | otherwise = (Player x y (max maxX x) (max maxY y) (water-1) (treasureCount))
updatePlayer (Player x y maxX maxY water treasureCount) (Water _ _ _ _) (GameRules _ _ canteenCapacity _ _) _= (Player x y (max maxX x) (max maxY y) canteenCapacity treasureCount)
updatePlayer (Player x y maxX maxY water treasureCount) (Lava _ _ _ _) _ _= (Player x y (max maxX x) (max maxY y) (water-1) treasureCount)
updatePlayer (Player x y maxX maxY water treasureCount) (Portal _ _ _ _) _ _= (Player x y (max maxX x) (max maxY y) (water-1) treasureCount)
updatePlayer (Player x y maxX maxY water treasureCount) _ _ _= (Player x y (max maxX x) (max maxY y) (water-1) treasureCount)

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
  in (filter (\x -> (not (sameKind (None (-1) (-1) False False) x) ) && (not (sameKind (Lava (-1) (-1) False False) x)) && (not (elem x visitedTiles))) [upperTile,rightTile,lowerTile,leftTile])

  
  