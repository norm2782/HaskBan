{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan.Types where
  import qualified Data.Map as M
  import Control.Monad (liftM, mapM_)
  import Control.Monad.State as MS
  import Data.Map (Map)

  data CellType = Wall
                | Player
                | Box
                | Path
                | Target (Maybe CellType) -- target could have a box on the initial state
                | Empty -- for spaces that don't mean anything on the map (see input)
                deriving (Eq, Ord)

  instance Show CellType where
    show Wall   = "#"
    show Player = "λ"
    show Box    = "$"
    show Path   = " "
    show (Target (Nothing)) = "."
    show (Target (Just _))  = "*"
    show Empty  = " "

  data Surrounding = Left CellType
                   | Right CellType
                   | Up CellType
                   | Down CellType
                   deriving (Show, Eq, Ord)
  
  type CellMatrix = [[CellType]]

  type Point = (Int, Int)
  type Translation = (Point -> Point)
  type SokoMap = M.Map Point CellType

  data SokobanStateInfo = SokobanStateInfo {
    player  :: Point,
    boxes   :: [Point],
    targets :: [Point],
    gameMap :: SokoMap
  } deriving (Show)

  -- QUESTION: on it's own Module?
  newtype SokobanState a = SokobanState (MS.State SokobanStateInfo a)
                           deriving (Monad, MonadState SokobanStateInfo)
