import System.Environment
import GameMap
import Serialization
import ParserAnalyser
import System.IO
import qualified GUI
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.IO.Unsafe
-- runhaskell desertExplorer.hs s 10 m 10 g 3 t 5  w 5 p 5 l 30 ll 30 x 4 y 3

main = do
  args <- getArgs
  --let args = ["s","10" ,"m", "10", "g", "3", "t", "5", "w", "5", "p", "2", "l", "30", "ll", "30", "x", "4", "y", "3"]    
  let rules = initializeRules args  
  if rules == Nothing
    then putStrLn "Problem with the parameters"
    else do       
      putStrLn "Would you like to start a new game (n) or load a previous one (l)?"
      command <- getChar
      startGame command rules
  
startGame :: Char -> Maybe GameRules -> IO()
startGame 'n' (Just rules) = do
  putStrLn "Starting a new game"  
  initializeGame (Just rules)

startGame 'l' (Just rules) = 
  do
    loadedGameState <- loadGameFromFile
    if loadedGameState == FailedGame
      then print "Problem with the game file. Please check it."
      -- else executeGame (loadedGameState)
      else guiExecuteGame (loadedGameState)

startGame _ rules = do
  print "Invalid command, please select a valid option (n) for creating a new game or (l) for loading a previous game."
  nCommand <- getChar
  startGame nCommand rules

initializeGame :: Maybe GameRules -> IO()
initializeGame (Just gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) = do   
  --                                                                                visitedTiles   Tcollected   revealedTiles             initialWorms
  -- executeGame (GameState (getBoard lineOfSight lineOfSight gameRules (Player 0 0 0 0 10 0) [(0,0)] [] (initializeRevealedRows 0 lineOfSight) [] 0) lineOfSight lineOfSight gameRules)
  guiExecuteGame (GameState (getBoard lineOfSight lineOfSight gameRules (Player 0 0 0 0 canteenCapacity 0) [(0,0)] [] (initializeRevealedRows 0 lineOfSight) [] 0) lineOfSight lineOfSight gameRules)


guiExecuteGame :: GameState -> IO ()
guiExecuteGame state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  do    
    -- play GUI.window GUI.background 1 state GUI.drawMap guiHandleCommand (\_ state -> state) 
    playIO GUI.window GUI.background 1 state GUI.drawMapIO guiHandleCommandIO (\_ state -> do return state) 


    
executeGame :: GameState -> IO()
executeGame state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) = 
  do
    printBoard board
    hSetBuffering stdin NoBuffering 
    command <- getChar
    if command /= 'e'
      then do 
        nState <- executeCommand state command
        executeGame nState 
        else do
          saveGame state
          executeGame state

guiHandleCommandIO :: Event -> GameState -> IO GameState
guiHandleCommandIO (EventKey (Char 'e') Down _ _) state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  do
    saveGame state
    return state

guiHandleCommandIO (EventKey (Char 'r') Down _ _) state@(GameState board@(Board tiles player _ _ _ _ _ _ _) rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability)) =
  do
    return (GameState (getBoard lineOfSight lineOfSight gameRules (Player 0 0 0 0 canteenCapacity 0) [(0,0)] [] (initializeRevealedRows 0 lineOfSight) [] 0) lineOfSight lineOfSight gameRules)

    -- in case the game is either won or lost, the state cannot change anymore
guiHandleCommandIO (EventKey _ _ _ _) state@(GameState board@(Board tiles player Lost _ _ _ _ _ _) rows columns gameRules) = 
  do
    return state

guiHandleCommandIO (EventKey _ _ _ _) state@(GameState board@(Board tiles player Won _ _ _ _ _ _) rows columns gameRules) = 
  do
    return state  
    
guiHandleCommandIO (EventKey (Char 'a') Down _ _) state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability))
  | (x-1) >= 0 =
    do
      nWorms <- filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability))
      return (GameState (getBoard rows columns gameRules (Player (x-1) y maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealLeftColumn (x-1) y lineOfSight)) nWorms (currentTurn+1)) (rows) columns gameRules)
  | otherwise = 
    do
      return state
    
guiHandleCommandIO (EventKey (Char 'w') Down _ _) state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability))
  | (y-1) >= 0 =
    do
      nWorms <- (filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability)))
      return (GameState (getBoard rows columns gameRules (Player x (y-1) maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealUpperRow x (y-1) lineOfSight)) nWorms (currentTurn+1)) (rows) columns gameRules)
  | otherwise = 
    do
      return state
    
guiHandleCommandIO (EventKey (Char 'd') Down _ _) state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules _ _ _ _ lineOfSight args  wormLength wormProbability)) =
  do
    let maxColumns = max (x+lineOfSight+1) columns
    nWorms <- (filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability)))
    return (GameState (getBoard rows maxColumns gameRules (Player (x+1) y maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealRightColumn (x+1) y lineOfSight)) nWorms (currentTurn+1)) rows maxColumns gameRules)

guiHandleCommandIO (EventKey (Char 's') Down _ _) state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules _ _ _ _ lineOfSight args  wormLength wormProbability)) =
  do
    let maxRows = max (y+lineOfSight+1) rows
    nWorms <- (filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability)))
    return (GameState (getBoard maxRows columns gameRules (Player x (y+1) maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealLowerRow (x) (y+1) lineOfSight)) nWorms (currentTurn+1)) maxRows columns gameRules)

guiHandleCommandIO _ state = 
  do 
    return state

saveGame :: GameState -> IO ()
saveGame state = do
  putStrLn "\n **************"
  putStrLn "\n **GAME SAVED**"
  putStrLn "\n **************"
  createGameFile (serializeGameState state)

saveGameBool :: GameState -> IO Bool
saveGameBool state = do
  createGameFileBool (serializeGameState state)
  
executeCommand :: GameState -> Char -> IO GameState
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability))  'a'
  | (x-1) >= 0 =
    do 
      nWorms <- filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability))
      return $ (GameState (getBoard rows columns gameRules (Player (x-1) y maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealLeftColumn (x-1) y lineOfSight)) nWorms (currentTurn+1)) (rows) columns gameRules)
  | otherwise = 
    do
      return state
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight args  wormLength wormProbability))  'w'
  | (y-1) >= 0 = 
    do
      nWorms <- filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability))
      return (GameState (getBoard rows columns gameRules (Player x (y-1) maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealUpperRow x (y-1) lineOfSight)) nWorms (currentTurn+1)) (rows) columns gameRules)
  | otherwise =
    do
      return state
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules _ _ _ _ lineOfSight args  wormLength wormProbability))  'd'  =
  do
    let maxColumns = max (x+lineOfSight+1) columns
    nWorms <- filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability))
    return (GameState (getBoard rows maxColumns gameRules (Player (x+1) y maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealRightColumn (x+1) y lineOfSight)) nWorms (currentTurn+1)) rows maxColumns gameRules)
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) rows columns gameRules@(GameRules _ _ _ _ lineOfSight args  wormLength wormProbability))  's'  =
  do
    let maxRows = max (y+lineOfSight+1) rows
    nWorms <- filterConcurrentWorms (awaitConcurrentWorms $ (simulateWormsConcurrent infiniteTiles worms wormLength wormProbability))
    return (GameState (getBoard maxRows columns gameRules (Player x (y+1) maxX maxY waterCount treasureCount) visitedTiles collectedTreasures (getNewRevealedTiles revealedTiles (revealLowerRow (x) (y+1) lineOfSight)) nWorms (currentTurn+1)) maxRows columns gameRules)
executeCommand state _ =
  do return state

getNewRevealedTiles :: RevealedTiles -> RevealedTiles -> RevealedTiles
getNewRevealedTiles previousTiles newTiles =
  let filteredNewTiles = filter (\x -> (not (elem x previousTiles))) newTiles
  in previousTiles++filteredNewTiles  
 

printBoard (Board tiles player@(Player xPos yPos maxX maxY water treasures) gameState visitedTiles collectedTreasures revealedTiles infiniteTiles worms currentTurn) = 
  do
    print '.' -- print a dot so the command doesnt affect the printing of the desert map
    mapM_ print (map (foldl (\acc x -> acc ++ show x) "") tiles)
    print ("Water left: "++(show water)++" |--| Treasures Collected: "++(show treasures)    )
    print "Magic Compass:"
    print (concat ["Closest Oasis ", show (useMagicCompass infiniteTiles (Water 0 0 False False False) yPos xPos)])
    print (concat ["Closest Portal ", show (useMagicCompass infiniteTiles (Portal 0 0 False False False) yPos xPos)])
    print (concat ["Closest Treasure ", show (useMagicCompass infiniteTiles (Treasure 0 0 False False False) yPos xPos)])
    putStrLn "Press e any time to save your game"    
    print currentTurn
    print maxY
    print maxY   


-------------------------MAIN MAP REVEALING FUNCTIONS--------------------------------------------
initializeRevealedRows currentRow lineOfSight
  | currentRow <= lineOfSight = (initializeRevealedColumns currentRow 0 lineOfSight)++(initializeRevealedRows (currentRow+1) lineOfSight)
  | otherwise = []

initializeRevealedColumns currentRow currentColumn lineOfSight
  | currentColumn <= lineOfSight = [(currentRow,currentColumn)]++(initializeRevealedColumns currentRow (currentColumn+1) lineOfSight)
  | otherwise = []

revealRightColumn posX posY lineOfSight =
  let lower = revealRightColumnDownwards posX posY posX posY lineOfSight
      upper = revealRightColumnUpwards posX posY posX posY lineOfSight
      filteredLower = filter (\x -> (not (elem x upper))) lower 
  in upper++filteredLower

revealLeftColumn posX posY lineOfSight =
  let lower = revealLeftColumnDownwards posX posY posX posY lineOfSight
      upper = revealLeftColumnUpwards posX posY posX posY lineOfSight
      filteredLower = filter (\x -> (not (elem x upper))) lower 
  in upper++filteredLower

revealLowerRow posX posY lineOfSight =
  let left = revealLowerLeftRow posX posY posX posY lineOfSight
      right = revealLowerRightRow posX posY posX posY lineOfSight
      filteredLeft = filter (\x -> (not (elem x right))) left 
  in right++filteredLeft

revealUpperRow posX posY lineOfSight =
  let left = revealUpperLeftRow posX posY posX posY lineOfSight
      right = revealUpperRightRow posX posY posX posY lineOfSight
      filteredLeft = filter (\x -> (not (elem x right))) left 
  in right++filteredLeft
-----------------------------------------------------------------  
-- revealing the columns
revealRightColumnDownwards :: Int -> Int -> Int -> Int -> Int -> RevealedTiles
revealRightColumnDownwards initialXPos initialYPos xPos yPos lineOfSight
  | (abs (yPos - initialYPos)) <= lineOfSight = [(yPos,(xPos+lineOfSight))]++revealRightColumnDownwards initialXPos initialYPos xPos (yPos+1) lineOfSight
  | otherwise = []

revealRightColumnUpwards :: Int -> Int -> Int -> Int -> Int -> RevealedTiles
revealRightColumnUpwards initialXPos initialYPos xPos yPos lineOfSight
  |yPos>=0 && (abs (yPos - initialYPos)) <= lineOfSight = [(yPos,(xPos+lineOfSight))]++revealRightColumnUpwards initialXPos initialYPos xPos (yPos-1) lineOfSight
  | otherwise = []

--revealLeftColumnDownwards :: Int -> Int -> Int -> Int -> Int -> RevealedTiles
revealLeftColumnDownwards initialXPos initialYPos xPos yPos lineOfSight
  | (xPos-lineOfSight)>=0 && (abs (yPos - initialYPos)) <= lineOfSight = [(yPos,(xPos-lineOfSight))]++revealLeftColumnDownwards initialXPos initialYPos xPos (yPos+1) lineOfSight
  | otherwise = []

--revealLeftColumnUpwards :: Int -> Int -> Int -> Int -> Int -> RevealedTiles
revealLeftColumnUpwards initialXPos initialYPos xPos yPos lineOfSight
  |(xPos-lineOfSight)>=0 && yPos>=0 && (abs (yPos - initialYPos)) <= lineOfSight = [(yPos,(xPos-lineOfSight))]++revealLeftColumnUpwards initialXPos initialYPos xPos (yPos-1) lineOfSight
  | otherwise = []


-- revealing the rows
revealLowerRightRow initialXPos initialYPos xPos yPos lineOfSight
  | (abs (xPos-initialXPos))<= lineOfSight = [((yPos+lineOfSight),xPos)]++revealLowerRightRow initialXPos initialYPos (xPos+1) yPos lineOfSight
  | otherwise = []

revealLowerLeftRow initialXPos initialYPos xPos yPos lineOfSight
  | xPos>=0 && (abs (xPos-initialXPos))<= lineOfSight = [((yPos+lineOfSight),xPos)]++revealLowerLeftRow initialXPos initialYPos (xPos-1) yPos lineOfSight
  | otherwise = []
  
-- upper row
revealUpperRightRow initialXPos initialYPos xPos yPos lineOfSight
  | (yPos-lineOfSight)>=0 &&  (abs (xPos-initialXPos))<= lineOfSight = [((yPos-lineOfSight),xPos)]++revealUpperRightRow initialXPos initialYPos (xPos+1) yPos lineOfSight
  | otherwise = []

revealUpperLeftRow initialXPos initialYPos xPos yPos lineOfSight
  |(yPos-lineOfSight)>=0 &&  xPos>=0 && (abs (xPos-initialXPos))<= lineOfSight = [((yPos-lineOfSight),xPos)]++revealUpperLeftRow initialXPos initialYPos (xPos-1) yPos lineOfSight
  | otherwise = []
