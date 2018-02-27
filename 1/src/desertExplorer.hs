import System.Environment
import GameMap
import System.IO

-- ["1","2","3","s","2","m","2","g","5","t","6","w","6","p","l","5","ll","2"]
-- ["m","2","g","5","t","6","w","6","p","3","l","5","ll","2"]
executionParams = ["m","g","t","w","p","l","ll"]

--                              Rows Cols
data GameState = GameState Board Int Int GameRules

main = do  
  args <- getArgs
  -- mapM_ print args
  -- putStrLn "The line fo sight is:"  
  -- putStrLn (args !! 0)
  let rules = initializeRules args
  
  if rules == Nothing
    then print "Problem with the parameters"
    else initializeGame rules
  -- let lineOfSight = 10
  -- let canteenCapacity = 10
  -- let seed = 3
  -- let normalProbabilities = [((Treasure 1 1 False),0.05), ((Water  1 1 False),0.1), ((Portal  1 1 False),0.05), ((Lava  1 1 False), 0.2), ((Desert  1 1 False), 0.6)]
  -- let lavaProbabilities = [((Treasure  1 1 False),0.18), ((Water  1 1 False),0.15), ((Portal  1 1 False),0.15), ((Lava  1 1 False), 0.1), ((Desert  1 1 False), 0.6)]
  -- let gameRules = (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight)
  -- executeGame (GameState (getBoard lineOfSight lineOfSight gameRules (Player 0 0 10 0) []) lineOfSight lineOfSight gameRules)
  
initializeGame :: Maybe GameRules -> IO()
initializeGame (Just gameRules@(GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight)) = executeGame (GameState (getBoard lineOfSight lineOfSight gameRules (Player 0 0 0 0 10 0) [(0,0)]) lineOfSight lineOfSight gameRules)
      
initializeRules :: [String] -> Maybe GameRules
-- initializeRules args = Just (GameRules [] [] 0 0 0)
initializeRules args
  | length args /= 16 || length (filter (\x -> (not (elem x args))) executionParams) > 0 = Nothing
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
        -- normal case
        normalDessertGeneralP = 1 - (w+p+l)
        normalTreasureP = normalDessertGeneralP*t
        normalDessertTileP = normalDessertGeneralP - normalTreasureP
        -- lava case
        lavaDessertGeneralP = 1 - (w+p+ll)
        lavaTreasureP = lavaDessertGeneralP*t
        lavaDessertTileP = lavaDessertGeneralP - lavaTreasureP        
    in if (w+p+l>1) || (w+p+ll>1)  then Nothing else Just (GameRules [((Treasure 1 1 False False),normalTreasureP) , ((Water 1 1 False False),w) , ((Portal 1 1 False False), p) , ((Desert 1 1 False False), normalDessertTileP), ((Lava 1 1 False False), l)] [((Treasure 1 1 False False),lavaTreasureP), ((Water 1 1 False False),w), ((Portal 1 1 False False), p), ((Desert 1 1 False False), lavaDessertTileP), ((Lava 1 1 False False), ll)] (round m) (round g) (round s))

fetchValue :: String -> [String] -> Float
fecthV  _ [] = 0.0
fetchValue name (p:v:rest) -- some error handling should go here
  | name == p = (read v :: Float)
  | otherwise = fetchValue name rest


executeGame :: GameState -> IO()
executeGame state@(GameState board@(Board tiles player _ _ _) rows columns (GameRules normalProbabilities lavaProbabilities canteenCapacity seed lineOfSight)) = do
  printBoard board
  hSetBuffering stdin NoBuffering 
  command <- getChar
  let nState = executeCommand state command
  executeGame nState 
-- executeGame state@(GameState board@(Board tiles player Normal _ _) rows columns gameRules) = do  
--   printBoard board
--   hSetBuffering stdin NoBuffering 
--   command <- getChar
--   let nState = executeCommand state command
--   executeGame nState 
-- executeGame state@(GameState board@(Board tiles player Lost _ _) rows columns gameRules) = do
--   print "Game Over, you lost!"
  
-- executeGame state@(GameState board@(Board tiles (Player x y _ _ water treasureCount) Won _ _) rows columns gameRules) = do
--   print "Game won!"
--   print ("Total of treasures collected: "++ (show treasureCount))
  
  
executeCommand :: GameState -> Char -> GameState
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles _) rows columns gameRules)  'a'
  | (x-1) >= 0 = (GameState (getBoard rows columns gameRules (Player (x-1) y maxX maxY waterCount treasureCount) visitedTiles) (rows) columns gameRules)
  | otherwise = state
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles _) rows columns gameRules)  'w'
  | (y-1) >= 0 = (GameState (getBoard rows columns gameRules (Player x (y-1) maxX maxY waterCount treasureCount) visitedTiles) (rows) columns gameRules)
  | otherwise = state
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles _) rows columns gameRules@(GameRules _ _ _ _ lineOfSight))  'd'  =
  let maxColumns = max (x+lineOfSight+1) columns
  in (GameState (getBoard rows maxColumns gameRules (Player (x+1) y maxX maxY waterCount treasureCount) visitedTiles) rows maxColumns gameRules)
executeCommand state@(GameState (Board tiles (Player x y maxX maxY waterCount treasureCount) gameStatus visitedTiles _) rows columns gameRules@(GameRules _ _ _ _ lineOfSight))  's'  =
  let maxRows = max (y+lineOfSight+1) rows
  in (GameState (getBoard maxRows columns gameRules (Player x (y+1) maxX maxY waterCount treasureCount) visitedTiles) maxRows columns gameRules)
executeCommand state _ = state
 

printBoard (Board tiles player@(Player xPos yPos _ _ water treasures) gameState visitedTiles infiniteTiles) = 
  do
    print '.' -- print a dot so the command doesnt affect the printing of the desert map
    mapM_ print (map (foldl (\acc x -> acc ++ show x) "") tiles)
    print ("Water left: "++(show water)++" |--| Treasures Collected so far: "++(show treasures)    )
    print "Magic Compass:"
    print (concat ["Closest Oasis ", show (useMagicCompass infiniteTiles (Water 0 0 False False) yPos xPos)])
    print (concat ["Closest Treasure ", show (useMagicCompass infiniteTiles (Treasure 0 0 False False) yPos xPos)])
    print (concat ["Closest Portal ", show (useMagicCompass infiniteTiles (Portal 0 0 False False) yPos xPos)])
