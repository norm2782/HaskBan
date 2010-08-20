module HaskBan where
  
  import SokoParser (parseSokoMap)
  import qualified Data.Map as M

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

  data SokobanState = SokobanState {
    player :: Point,
    boxes :: [Point],
    targets :: [Point],
    cellMap :: SokoMap
  }


