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
    initCurses
    echo False
    keypad stdScr True
    cursSet CursorInvisible
    mvWAddStr stdScr 0 0 "Welcome to HaskBan, the world's most awesome Haskell-based Sokoban game."
    showMoves 0
    refresh
    progLoop

  showMoves :: Int -> IO ()
  showMoves s = mvWAddStr stdScr 3 30 ("Number of moves: " ++ show s)

  progLoop :: IO ()
  progLoop = do --mvWAddStr stdScr 1 0 (showSokoMap (return getMap))
                key <- getCh
                if shouldTerminate key
                  then do endWin
                  else do return (keyPressed key)
                          progLoop

  keyPressed :: Key -> SokobanState()
  keyPressed key = do map <- getMap
                      pos <- getPlayerPosition
                      let transl = getTranslation key
                      if canMoveTo map pos transl
                        then do movePlayer map transl
                                when (isBox (transl pos) map) (moveBox map (transl pos) transl)
                        else return ()
  
  getTranslation :: Key -> Translation
  getTranslation key | key == KeyUp    || key == (KeyChar 'k') = translateUp
                     | key == KeyDown  || key == (KeyChar 'j') = translateDown
                     | key == KeyLeft  || key == (KeyChar 'h') = translateLeft
                     | key == KeyRight || key == (KeyChar 'l') = translateRight
  
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
  isCellType cType point sMap = getCellType point sMap == cType

  getCellType :: Point -> SokoMap -> CellType
  getCellType point sMap = sMap ! point

  getPlayerPosition :: SokobanState Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: Point -> SokobanState ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  getMap :: SokobanState SokoMap
  getMap = sokoMap `liftM` get 

  putMap :: SokoMap -> SokobanState ()
  putMap map = get >>= \state -> put (state {sokoMap = map})

  movePlayer :: SokoMap -> Translation -> SokobanState ()
  movePlayer sMap transl = liftM transl getPlayerPosition >>= \position ->
                           when (canMoveTo sMap position transl) (putPlayerPosition position)

  moveBox :: SokoMap -> Point -> Translation -> SokobanState()
  moveBox = undefined

  -- Verify if the player can move to the point that is provided.
  -- In case the new pointis a box, the next position needs to be
  -- checked as well. Hence, the original translation function is provided as well.
  canMoveTo :: SokoMap -> Point -> Translation -> Bool
  canMoveTo sMap point transl | isPath point sMap = True
                              | isBox  point sMap && not (isWall (transl point) sMap) = True
                              | otherwise  = False

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 
