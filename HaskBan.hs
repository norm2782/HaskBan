{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan where
  
--  import SokoParser (parseSokoMap)
  import qualified Data.Map as M
  import Control.Monad.State as MS
  import Control.Monad (liftM, mapM_)

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


