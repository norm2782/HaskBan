{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module HaskBan (main) where

  -- ncurses (gui) library
  import UI.HSCurses.Curses
  import HaskBan.Curses
  
  -- haskban internals
  import HaskBan.Types 
  import HaskBan.Parser (parseSokoMaps)
  import HaskBan.Printer 

  -- others
  import Control.Monad (mapM_, liftM, forever)
  import Control.Monad.State
  import qualified Data.ByteString as BS
  import qualified Data.IntMap as IM ((!))
  import Data.Map ((!), Map)
  
  main :: SokobanMonad ()
  main = liftIO $ do
    window <- setupHaskBanGUI 
    -- mvWAddStr window 2 0 (showSokoMap (sokoMaps IM.! 0))
    --cursSet CursorInvisible
    showMoves 0
    refresh
    forever (readKeyAndPrint window)
    
    -- progLoop

  readKeyAndPrint :: Window -> IO ()
  readKeyAndPrint window = liftIO $ do
    key <- getCh
    if shouldTerminate key
      then endWin
      else do
        wMove window 20 0
        wAddStr window (show key)

  showMoves :: Int -> IO ()
  showMoves s = mvWAddStr stdScr 3 30 ("Number of moves: " ++ show s)

  --progLoop :: IO ()
  --progLoop = do --mvWAddStr stdScr 1 0 (showSokoMap (return getMap))
  --              key <- getCh
  --              if shouldTerminate key
  --                then do endWin
  --                else do return (keyPressed key)
  --                        progLoop

  keyPressed :: (MonadState SokobanInfo) m => Key -> m ()
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

  getPlayerPosition :: (MonadState SokobanInfo) m => m Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: (MonadState SokobanInfo) m => Point -> m ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  getMap :: (MonadState SokobanInfo) m => m SokoMap
  getMap = sokoMap `liftM` get 

  putMap :: (MonadState SokobanInfo) m => SokoMap -> m ()
  putMap map = get >>= \state -> put (state {sokoMap = map})

  movePlayer :: (MonadState SokobanInfo) m => SokoMap -> Translation -> m ()
  movePlayer g t = liftM t getPlayerPosition >>= \position ->
                   when (canMoveTo g position t) (putPlayerPosition position)

  moveBox :: (MonadState SokobanInfo) m => SokoMap -> Point -> Translation -> m ()
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

