module Serialization
where
import GameMap
import System.Directory
import System.Posix.IO


import Data.Time
import Data.Time.Format

serializeGameState :: GameState -> String
serializeGameState state = 
  produceExplorerPosition state  ++ produceExplorerWaterSupply state ++ produceExplorerLineOfSight state ++ produceExplorerWaterCapacity state ++ produceSeed state ++ produceTreasureProb state ++ produceWaterProb state ++ producePortalProb state ++ produceLavaProb state ++ produceAdjacentLava state ++ produceEmergingWorms state ++ produceDissapearingWorms state ++ produceCollectedTreasures state++ produceRevealedTiles state ++ produceWormLength state ++ produceWormProb state

createGameFile :: String -> IO ()
createGameFile game = do
  currentDirectoryPath <- getCurrentDirectory
  let nDirectoryFilepath = (currentDirectoryPath++"/savedGames/")  
  createDirectoryIfMissing False nDirectoryFilepath
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  let (year, month, day) = toGregorian $ utctDay now
  let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
  --putStrLn $ (show year)++"/"++(show month)++"/"++(show day)++"-"++(show hour)++":"++(show minute)++":"++(show second)
  let fileName = (show year)++"_"++(show month)++"_"++(show day)++"_"++(show hour)++"_"++(show minute)++"_"++(show second)
  --let nFilePath = nDirectoryFilepath++fileName++".txt"
  let nFilePath = nDirectoryFilepath++"saved.txt"
  -- createFile nFilePath 1
  writeFile nFilePath game
  putStrLn "\n **************"
  putStrLn "\n **GAME SAVED**"
  putStrLn "\n **************"

createGameFileBool :: String -> IO Bool
createGameFileBool game = do
  currentDirectoryPath <- getCurrentDirectory
  let nDirectoryFilepath = (currentDirectoryPath++"/savedGames/")  
  createDirectoryIfMissing False nDirectoryFilepath    
  let nFilePath = nDirectoryFilepath++"saved.txt"
  -- createFile nFilePath 1
  writeFile nFilePath game
  return True

producePosition :: Int -> Int -> String
producePosition row column = "["++(produceNatural row)++","++(produceNatural column)++"]"

producePositions :: (Int,Int) -> String
producePositions (row,column) =  " , ["++(produceNatural row)++","++(produceNatural column)++"]"

produceNatural :: Int -> String
produceNatural n = " "++(show n)++" "

produceExplorerPosition :: GameState -> String
produceExplorerPosition (GameState (Board _ (Player x y _ _ _ _) _ _ _ _ _ _ _) _ _ _) = "position ( "++(producePosition y x)++ " ) "

produceExplorerWaterSupply :: GameState -> String
produceExplorerWaterSupply (GameState (Board _ (Player _ _ _ _ waterCount _) _ _ _ _ _ _ _) _ _ _) = "\n supply ("++(produceNatural waterCount)++ ")"

produceExplorerLineOfSight :: GameState -> String
produceExplorerLineOfSight (GameState _ _ _ (GameRules _ _ _ _ lineOfSight _ _ _)) = "\n s ("++(produceNatural lineOfSight)++ ")"

produceExplorerWaterCapacity :: GameState -> String
produceExplorerWaterCapacity (GameState _ _ _ (GameRules _ _ canteenCapacity _ _ _ _ _))= "\n m ("++(produceNatural canteenCapacity)++ ")"

produceSeed :: GameState -> String
produceSeed (GameState _ _ _ (GameRules _ _ _ seed _ _ _ _))= "\n g ("++(produceNatural seed)++ ")"

produceTreasureProb :: GameState -> String
produceTreasureProb (GameState _ _ _ (GameRules _ _ _ _ _ args _ _)) =
  let treasureProb = produceNatural (round (fetchValue "t" args))
  in "\n t ("++treasureProb++ ")"

produceWaterProb :: GameState -> String
produceWaterProb (GameState _ _ _ (GameRules _ _ _ _ _ args _ _)) =
  let water = produceNatural (round (fetchValue "w" args))
  in "\n w ("++water++ ")"


producePortalProb :: GameState -> String
producePortalProb (GameState _ _ _ (GameRules _ _ _ _ _ args _ _)) =
  let portal = produceNatural (round (fetchValue "p" args))
  in "\n p ("++portal++ ")"


produceLavaProb :: GameState -> String
produceLavaProb (GameState _ _ _ (GameRules _ _ _ _ _ args _ _)) =
  let lava = produceNatural (round (fetchValue "l" args))
  in "\n l ("++lava++ ")"

produceAdjacentLava :: GameState -> String
produceAdjacentLava (GameState _ _ _ (GameRules _ _ _ _ _ args _ _)) =
  let adjacentLava = produceNatural (round (fetchValue "ll" args))
  in "\n ll ("++adjacentLava++ ")"

produceWormLength :: GameState -> String
produceWormLength (GameState _ _ _ (GameRules _ _ _ _ _ _ wormLength _)) =
  let nWormLength = produceNatural wormLength
  in "\n x ("++nWormLength++ ")"


produceWormProb :: GameState -> String
produceWormProb (GameState _ _ _ (GameRules _ _ _ _ _ _ _ wormProb)) =
  let nWormProb = produceNatural (round (wormProb*100))
  in "\n y ("++nWormProb++ ")"

produceEmergingWorms :: GameState -> String
produceEmergingWorms (GameState (Board _ _ _ _ _ _ _ worms _) _ _ _) = foldl (\acc x -> acc++x) "" (map produceWorm (filter isEmergingWorm worms))

produceDissapearingWorms :: GameState -> String
produceDissapearingWorms (GameState (Board _ _ _ _ _ _ _ worms _) _ _ _) = foldl (\acc x -> acc++x) "" (map produceWorm (filter (\x -> (not (isEmergingWorm x))) worms))

produceWorm :: Worm -> String
produceWorm (EmergingWorm ((hx,hy):t)) = "\n emerging ( "++(producePosition hx hy)++(foldl (\acc x -> acc++x) "" (map producePositions t))++" )"
produceWorm (DissapearingWorm ((hx,hy):t)) = "\n disappearing ( "++(producePosition hx hy)++(foldl (\acc x -> acc++x) "" (map producePositions t))++" )"

produceCollectedTreasures :: GameState -> String
produceCollectedTreasures (GameState (Board _ _ _ _ collectedTreasures _ _ _ _) _ _ _) = foldl (\acc x -> acc++x) "" (map (\(row,column) -> "\n collected ( "++(producePosition row column)++" )" ) collectedTreasures)

produceRevealedTiles :: GameState -> String
produceRevealedTiles (GameState (Board _ _ _ _ _ revealedTiles _ _ _) _ _ _) = foldl (\acc x -> acc++x) "" (map (\(row,column) -> "\n revealed ( "++(producePosition row column)++" )" ) revealedTiles)