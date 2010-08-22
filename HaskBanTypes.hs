module HaskBanTypes where
  import qualified Data.Map as M

  data CellType = Wall
                | Player
                | Box
                | Path
                | Target (Maybe CellType) -- target could have a box on the initial state
                | Empty -- for spaces that don't mean anything on the map (see input)
                deriving (Eq, Ord)

  data Surrounding = Left CellType
                   | Right CellType
                   | Up CellType
                   | Down CellType
                   deriving (Eq, Ord)
  
  type CellMatrix = [[CellType]]

  type Point = (Int, Int)

  type SokoMap = M.Map Point CellType

  data SokobanState = SokobanState {
    player :: Point,
    boxes :: [Point],
    targets :: [Point],
    cellMap :: SokoMap
  }

  -- QUESTION: on it's own Module?
  newtype SokobanState a = SokobanState (MS.State SokobanStateInfo a)
                           deriving (Monad, MonadState SokobanStateInfo)

  getPlayerPosition :: SokobanState Point
  getPlayerPosition = player `liftM` get

  putPlayerPosition :: Point -> SokobanState ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  
