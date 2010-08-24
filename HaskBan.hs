{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan (main) where
  
  import UI.HSCurses.Curses
  import HaskBan.Types
  import HaskBan.Parser (parseSokoMaps)
  import HaskBan.Printer 
  import Control.Monad (mapM_, liftM)
  import Control.Monad.State
  import qualified Data.ByteString as BS
  import Data.Map ((!), Map)
  
  main :: IO ()
  main = do 
    contents <- BS.readFile "input.in" 
    window <- initScr
    initCurses
    mvWAddStr window 0 0 "Welcome to HaskBan, the world's most awesome Haskell-based Sokoban game."
    mapM_ ((mvWAddStr window 1 0) . showCellMatrix) (runHaskBanParser contents)
    refresh
    progLoop

  progLoop :: IO ()
  progLoop = do key <- getCh
                if shouldTerminate key
                  then do endWin
                  else do return (keyPressed key)
                          progLoop

  keyPressed :: Key -> SokobanState()
  keyPressed k = do map <- getMap
                    pos <- getPlayerPosition
                    let t = getTranslation k
                    if canMoveTo map pos t
                      then do movePlayer map t
                              if isBox (t pos) map
                                then moveBox map (t pos) t
                                else return ()
                      else return ()
  
  getTranslation :: Key -> Translation
  getTranslation KeyUp    = translateUp
  getTranslation KeyDown  = translateDown
  getTranslation KeyLeft  = translateLeft
  getTranslation KeyRight = translateRight

  translateUp :: Translation
  translateUp (x, y)    = (x, y - 1)

  translateDown :: Translation
  translateDown (x, y)  = (x, y + 1)

  translateLeft :: Translation
  translateLeft (x, y)  = (x - 1, y)

  translateRight :: Translation
  translateRight (x, y) = (x + 1, y)
  
  isWall :: Point -> SokoMap -> Bool
  isWall = isCellType Wall

  isBox :: Point -> SokoMap -> Bool
  isBox = isCellType Box

  isPath :: Point -> SokoMap -> Bool
  isPath = isCellType Path

  isCellType :: CellType -> Point -> SokoMap -> Bool
  isCellType c p m = getCellType p m == c

  getCellType :: Point -> SokoMap -> CellType
  getCellType p m = m ! p

  getPlayerPosition :: SokobanState Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: Point -> SokobanState ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  getMap :: SokobanState SokoMap
  getMap = sokoMap `liftM` get 

  putMap :: SokoMap -> SokobanState ()
  putMap map = get >>= \state -> put (state {sokoMap = map})

  movePlayer :: SokoMap -> Translation -> SokobanState ()
  movePlayer g t = liftM t getPlayerPosition >>= \position ->
                   when (canMoveTo g position t) (putPlayerPosition position)

  moveBox :: SokoMap -> Point -> Translation -> SokobanState()
  moveBox = undefined

  -- Verify if the player can move to the point that is provided.
  -- In case the new pointis a box, the next position needs to be
  -- checked as well. Hence, the original translation function is provided as well.
  canMoveTo :: SokoMap -> Point -> Translation -> Bool
  canMoveTo g p t | isPath p g = True
                  | isBox p g && not (isWall (t p) g) = True
                  | otherwise  = False

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 
