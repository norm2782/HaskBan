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

  walkUp :: Point -> Point
  walkUp (x, y)    = (x, y - 1)

  walkDown :: Point -> Point
  walkDown (x, y)   = (x, y + 1)

  walkLeft :: Point -> Point
  walkLeft (x, y)  = (x - 1, y)

  walkRight :: Point -> Point
  walkRight (x, y) = (x + 1, y)
  
--  isWall :: Point -> Map -> Bool
  --isWall p m = isCellType p Wall m 

 -- isBox :: Point -> Map -> Bool
 -- isBox p m :: isCellType p Box m

--  isCellType :: Point -> CellType -> Map -> Bool
--  isCellType (x, y) c m = Map.elemAt y (snd (Map.elemAt x m)) == c

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

