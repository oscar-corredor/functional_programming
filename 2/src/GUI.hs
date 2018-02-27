module GUI
where
import System.Environment
import GameMap
import Serialization
import ParserAnalyser
import System.IO
import Graphics.Gloss

window :: Display
window = FullScreen

background :: Color
background = black

drawMap :: GameState -> Picture
drawMap state@(GameState board@(Board tiles player Lost _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  pictures $ produceGameOverPictures "Game over, you lost!" state

drawMap state@(GameState board@(Board tiles player Won _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  pictures $ produceGameOverPictures "Congratulations, you won!" state
  
drawMap state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  pictures $ producePictures state

  
-- monadic version of the GUI  
drawMapIO :: GameState -> IO Picture
drawMapIO state@(GameState board@(Board tiles player@(Player x y _ _ _ _)  Lost _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  do    
    return (pictures $ produceGameOverPictures (lostGameMessage player (tiles!!y!!x)) state)

drawMapIO state@(GameState board@(Board tiles player Won _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  do  
    return (pictures $ produceGameOverPictures "Congratulations, you won!" state)
  
drawMapIO state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  do
    return (pictures $ producePictures state)
  

lostGameMessage :: Player -> Tile -> String
lostGameMessage (Player x y _ _ water _) tile
  | (getHasWorm tile) = "Stepped on a worm!"
  | (sameKind (Lava 0 0 False False False) tile) = "Stepped on lava!"
  | water <= 0 = "Ran out of water!"
  | otherwise = "No idea what happened m8. As far as I know you should still be alive :p"
  

produceTilePicture :: String -> Int -> Int -> Int -> Picture
produceTilePicture tileString tileSize row column
  | tileString == " . " =  (positionPicture row column tileSize) (color desertColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | tileString == " T " =  (positionPicture row column tileSize) (color treasureColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | tileString == " ~ " =  (positionPicture row column tileSize) (color waterColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | tileString == " * " =  (positionPicture row column tileSize) (color lavaColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | tileString == " @ " =  (positionPicture row column tileSize) (color portalColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize)   )
  | tileString == " < " =  (positionPicture row column tileSize) (color wormColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | tileString == "[P]" =  (positionPicture row column tileSize) (color playerColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | tileString == "   " =  (positionPicture row column tileSize) (color emptyColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize) )
  | otherwise           =  (positionPicture row column tileSize) (color otherwiseColor $ rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize))

producePictures :: GameState -> [Picture]
producePictures state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  (produceText state) ++ map (\tile -> produceTilePicture (show tile) mapCellSize (getRowCoordinate tile) (getColumnCoordinate tile)) (concat tiles)

positionPicture :: Int -> Int -> Int -> Picture -> Picture
positionPicture row column tileSize = translate ((fromIntegral(column*tileSize)) - 700) (350 - (fromIntegral(row*tileSize)))

produceText :: GameState -> [Picture]
produceText state@(GameState board@(Board tiles player@(Player xPos yPos maxX maxY water treasures) gameState visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  let playerStatus =  ("Water left: "++(show water)++" |--| Treasures Collected: "++(show treasures))
      compass = "Compass:"++(concat [" Oasis ", show (useMagicCompass infiniteTiles (Water 0 0 False False False) yPos xPos)])++(concat [" Portal ", show (useMagicCompass infiniteTiles (Portal 0 0 False False False) yPos xPos)])++(concat [" Treasure ", show (useMagicCompass infiniteTiles (Treasure 0 0 False False False) yPos xPos)])
      message = "Press e anytime to save your progress"

  in [(produceTextPicture playerStatus(-700) (400)),(produceTextPicture compass (-700) (380)),(produceTextPicture message (-700) (360))]

produceTextPicture :: String -> Int -> Int -> Picture
produceTextPicture string column row = translate (fromIntegral column) (fromIntegral row)  $ (scale 0.15 0.15 (color white $ text string))

produceGameOverPictures :: String -> GameState -> [Picture]
produceGameOverPictures message state@(GameState board@(Board _ (Player _ _ _ _ _ treasures) _ _ _ _ _ _ _) _ _ _) =
  let firstMessage = message
      secondMessage = "Total treasures collected: "++(show treasures)
      thirdMessage = "Press r to restart the game."
  in [(produceTextPicture firstMessage (-700) (400)),(produceTextPicture secondMessage (-700) (380)),(produceTextPicture thirdMessage (-700) (360))]


desertColor = yellow
treasureColor = red
waterColor = light blue
lavaColor = orange
portalColor = white
wormColor = magenta
playerColor = violet
emptyColor = black
otherwiseColor = cyan

mapCellSize  = 20
mapCellSpace  = 1
mapHeight  =  100
mapWidth  = 100
