{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan (mainAction, jurrenMainAction) where
  
  import UI.HSCurses.Curses
  import HaskBanTypes
  import HaskBanParser (runHaskBanParser)
  import HaskBanPrinter 
  import Control.Monad (mapM_)
  import qualified Data.ByteString as BS
  
  mainAction :: IO ()
  mainAction = BS.readFile "input.in" >>= \contents ->
               mapM_ (putStrLn . showCellMatrix) (runHaskBanParser contents)

  main :: IO ()
  main = do window <- initScr
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

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 

  progLoop :: IO ()
  progLoop = do key <- getCh
                if shouldTerminate key
                  then do endWin
                  else do return (processKey key)
                          progLoop

