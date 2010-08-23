{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan (main) where
  
  import UI.HSCurses.Curses
  import HaskBan.Types
  import HaskBan.Parser (runHaskBanParser)
  import HaskBan.Printer 
  import Control.Monad (mapM_, liftM)
  import Control.Monad.State
  import qualified Data.ByteString as BS
  
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

