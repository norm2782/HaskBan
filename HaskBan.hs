{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module HaskBan (main) where

  -- ncurses (gui) library
  import UI.HSCurses.Curses
  
  -- haskban internals
  import HaskBan.Curses
  import HaskBan.Logic
  import HaskBan.Monad
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
  main = do
    window <- liftIO setupHaskBanGUI
    liftIO $ showMoves window 0
    liftIO refresh
    forever (readKeyAndPrint window)

  readKeyAndPrint :: Window -> SokobanMonad ()
  readKeyAndPrint window = do
    displaySokobanMap window
    key <- liftIO getCh
    if shouldTerminate key
      then liftIO endWin
      else do
        keyPressed key
        incrNumberOfSteps
        liftIO $ mvWAddStr window 20 0 (show key)
        steps <- getNumberOfSteps
        liftIO $ showMoves window steps
        liftIO $ refresh
        -- sokobanInfo <- get
        -- liftIO $ mvWAddStr window 30 0 (show sokobanInfo)

  showMoves :: Window -> Int -> IO ()
  showMoves w s = mvWAddStr w 3 30 ("Number of key-presses: " ++ show s)

  -- keyPressed :: (MonadState SokobanInfo) m => Key -> m ()
  keyPressed :: Key -> SokobanMonad ()
  keyPressed k = do 
    let t = getTranslation k
    movePlayer t
    return ()
  
  getTranslation :: Key -> Translation
  getTranslation key | key == KeyUp    || key == (KeyChar 'k') = translateUp
                     | key == KeyDown  || key == (KeyChar 'j') = translateDown
                     | key == KeyLeft  || key == (KeyChar 'h') = translateLeft
                     | key == KeyRight || key == (KeyChar 'l') = translateRight
                     | otherwise                               = id 

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 

  displaySokobanMap :: Window -> SokobanMonad ()
  displaySokobanMap window = do
    sokobanMap <- getMap
    liftIO $ do
      let sokoMapStr = showSokoMap sokobanMap
      mvWAddStr window 2 0 sokoMapStr 
      refresh
