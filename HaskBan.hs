{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan (main) where
  
  import UI.HSCurses.Curses
  import HaskBan.Types
  import HaskBan.Parser (runHaskBanParser)
  import HaskBan.Printer 
  import Control.Monad (mapM_, liftM)
  import Control.Monad.State
  import qualified Data.ByteString as BS
  import Data.Map ((!), Map)
  
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
  
  isWall :: Point -> GameMap -> Bool
  isWall = isCellType Wall

  isBox :: Point -> GameMap -> Bool
  isBox = isCellType Box

  isPath :: Point -> GameMap -> Bool
  isPath = isCellType Path

  isCellType :: CellType -> Point -> GameMap -> Bool
  isCellType c p m = getCellType p m == c

  getCellType :: Point -> GameMap -> CellType
  getCellType (x, y) m = (m ! y) ! x

  getPlayerPosition :: SokobanState Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: Point -> SokobanState ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  movePlayer :: GameMap -> (Point -> Point) -> SokobanState ()
  movePlayer g t = liftM t getPlayerPosition >>= \position ->
                   when (canMoveTo g position t) (putPlayerPosition position)

  canMoveTo :: GameMap -> Point -> (Point -> Point) -> Bool
  canMoveTo g p t | isPath p g = True
                  | isBox p g && not (isWall (t p) g) = True
                  | otherwise = False

  shouldTerminate :: Key -> Bool
  shouldTerminate (KeyChar '\ESC') = True
  shouldTerminate _                = False 

  progLoop :: IO ()
  progLoop = do key <- getCh
                if shouldTerminate key
                  then do endWin
                  else do return (processKey key)
                          progLoop

