{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module HaskBan.Types where

  import Data.IntMap (IntMap)
  import Data.Map (Map)

  data InnerCell = Player
                 | Box
                 | Empty
                 deriving (Eq)
  
  data CellType = Wall
                | Path   { inner :: InnerCell }
                | Target { inner :: InnerCell }
                deriving (Eq)

  instance Show CellType where
    show Wall            = "#"
    show (Path Player)   = "P"
    show (Path Box)      = "$"
    show (Path Empty)    = " "
    show (Target Empty)  = "."
    show (Target Player) = "P"
    show (Target Box)    = "*"

  type CellMatrix = [[CellType]]

  type Point       = (Int, Int)
  type Translation = (Point -> Point)
  type SokoMap     = Map Point CellType
  type SokoMaps    = IntMap SokoMap

  data SokobanInfo = SokobanInfo {
    currentLevel :: Int,
    player  :: Point,
    boxes   :: [Point],
    targets :: [Point],
    sokoMap :: SokoMap
  } deriving (Show)



