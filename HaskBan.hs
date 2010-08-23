{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan (main) where
  
  import UI.HSCurses.Curses
  import HaskBanTypes
  import HaskBanParser (runHaskBanParser)
  import HaskBanPrinter 
  import Control.Monad (mapM_, liftM)
  import Control.Monad.State
  import qualified Data.ByteString as BS
  import Data.Map (Map)
  import qualified Data.Map as Map
  
  main :: IO ()
  main = do 
    contents <- BS.readFile "input.in" 
    mapM_ (putStrLn . showCellMatrix) (runHaskBanParser contents)
    window <- initScr
    initCurses
    mvWAddStr window 0 0 "Welcome to HaskBan, the world's most awesome Haskell-based Sokoban game."
    move 1 0
    refresh
    progLoop

  processKey :: Key -> ()
  processKey KeyUp    = undefined
  processKey KeyDown  = undefined
  processKey KeyLeft  = undefined
  processKey KeyRight = undefined
  processKey _        = undefined

  translateUp :: Point -> Point
  translateUp (x, y)    = (x, y - 1)

  translateDown :: Point -> Point
  translateDown (x, y)  = (x, y + 1)

  translateLeft :: Point -> Point
  translateLeft (x, y)  = (x - 1, y)

  translateRight :: Point -> Point
  translateRight (x, y) = (x + 1, y)
  
--  isWall :: Point -> Map -> Bool
--  isWall = isCellType Wall

--  isBox :: Point -> Map -> Bool
--  isBox :: isCellType Box

--  isCellType :: CellType -> Point -> Map -> Bool
  isCellType c p m = getCellType p m == c

--  getCellType :: Point -> Map -> CellType
  getCellType (x, y) m = Map.elemAt y (snd (Map.elemAt x m))

  getPlayerPosition :: SokobanState Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: Point -> SokobanState ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 

  progLoop :: IO ()
  progLoop = do key <- getCh
                if shouldTerminate key
                  then do endWin
                  else do return (processKey key)
                          progLoop

