{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan where
  
--  import SokoParser (parseSokoMap)
  import qualified Data.Map as M
  import Control.Monad.State as MS
  import Control.Monad (liftM, mapM_)
  import UI.HSCurses.Curses

  data CellType = Wall
                | Box
                | Path
                | Target
                deriving (Show, Eq, Ord)

  data Surrounding = Left CellType
                   | Right CellType
                   | Up CellType
                   | Down CellType
                   deriving (Show, Eq, Ord)
  
  type Point = (Int, Int)

  type SokoMap = M.Map Point CellType

  data SokobanStateInfo = SokobanStateInfo {
    player  :: Point,
    boxes   :: [Point],
    targets :: [Point],
    cellMap :: SokoMap
  } deriving (Show)

  newtype SokobanState a = SokobanState (MS.State SokobanStateInfo a)
                           deriving (Monad, MonadState SokobanStateInfo)

  getPlayerPosition :: SokobanState Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: Point -> SokobanState ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  processKey = undefined

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 

  progLoop :: IO ()
  progLoop = do key <- getCh
                if shouldTerminate key
                  then do endWin
                  else do return (processKey key)
                          progLoop

  main :: IO ()
  main = do window <- initScr
            initCurses
            mvWAddStr window 0 0 "Welcome to HaskBan, the world's most awesome Haskell-based Sokoban game."
            move 1 0
            refresh
            progLoop
