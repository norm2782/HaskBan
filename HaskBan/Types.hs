{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan.Types where

  import qualified Data.Map as M
  import Control.Monad (liftM, mapM_)
  import Control.Monad.State as MS
  import Data.IntMap (IntMap)
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
    show Player = "P"
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
  type SokoMap = Map Point CellType
  type SokoMaps = IntMap SokoMap

  data SokobanInfo = SokobanInfo {
    currentLevel :: Int,
    player  :: Point,
    boxes   :: [Point],
    targets :: [Point],
    sokoMap :: SokoMap
  } deriving (Show)

  newtype SokobanMonad a = SokobanMonad (MS.StateT SokobanInfo IO a)
                           deriving (Monad, MonadIO, MonadState SokobanInfo)

  runSokobanMonad :: SokobanMonad a -> SokobanInfo -> IO (a, SokobanInfo)
  runSokobanMonad (SokobanMonad stateT) initialState = runStateT stateT initialState

