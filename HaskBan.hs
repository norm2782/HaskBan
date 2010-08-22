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

  jurrenMainAction :: IO ()
  jurrenMainAction = do 
    window <- initScr
    initCurses
    progLoop

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

  
  
