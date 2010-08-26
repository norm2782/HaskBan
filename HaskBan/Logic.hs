module HaskBan.Logic where
  
  import HaskBan.Types 
  import Data.Map ((!))

  updateList :: (Eq a) => (a -> a) -> a -> [a] -> [a]
  updateList _ _ [] = []
  updateList fn x (y:ys) 
    | x == y = ((fn x):ys)
    | otherwise = (y:updateList fn x ys)

  translateUp :: Translation
  translateUp (row, column)    = (row - 1, column)

  translateDown :: Translation
  translateDown (row, column)  = (row + 1, column)

  translateLeft :: Translation
  translateLeft (row, column)  = (row, column - 1)

  translateRight :: Translation
  translateRight (row, column) = (row, column + 1)
  
  isWall :: Point -> SokoMap -> Bool
  isWall = isCellType Wall

  isBox :: Point -> SokoMap -> Bool
  isBox p sm = (isCellType (Path Box)   p sm) ||
               (isCellType (Target Box) p sm)

  isPath :: Point -> SokoMap -> Bool
  isPath = isCellType (Path Empty)

  isTarget :: Point -> SokoMap -> Bool
  isTarget p sm = isCellType (Target Box)   p sm ||
                  isCellType (Target Empty) p sm 

  isCellType :: CellType -> Point -> SokoMap -> Bool
  isCellType cType point sMap = (sMap ! point) == cType

  -- Verify if the player can move to the point that is provided.
  -- In case the new pointis a box, the next position needs to be
  -- checked as well. Hence, the original translation function is provided as well.
  canMoveTo :: SokoMap -> Point -> Translation -> Bool
  canMoveTo sMap point transl = 
    let
      point'  = transl point
      ct  = (sMap ! point)
      ct' = (sMap ! point')
    in
      checkCellTypesToMove sMap point' transl ct ct'
                               
                              
                             
                            

  checkCellTypesToMove :: SokoMap -> Point -> Translation -> CellType -> CellType -> Bool
  -- anything that goes to a wall is just False
  checkCellTypesToMove _ _ _ _ Wall = False
  --
  checkCellTypesToMove _ _ _ (Path Empty)  (Path Empty) = True
  checkCellTypesToMove _ _ _ (Target Empty) (Target Empty) = True
  checkCellTypesToMove _ _ _ (Target Empty) (Path Empty) = True
  checkCellTypesToMove _ _ _ (Path Empty) (Target Empty) = True
  -- player to empty is true in any cell type
  checkCellTypesToMove _ _ _ (Path Player)  (Path Empty) = True
  checkCellTypesToMove _ _ _ (Target Player) (Target Empty) = True
  checkCellTypesToMove _ _ _ (Target Player) (Path Empty) = True
  checkCellTypesToMove _ _ _ (Path Player) (Target Empty) = True
  -- box to empty is true in any cell type
  checkCellTypesToMove _ _ _ (Path Box)  (Path Empty) = True
  checkCellTypesToMove _ _ _ (Target Box) (Target Empty) = True
  checkCellTypesToMove _ _ _ (Target Box) (Path Empty) = True
  checkCellTypesToMove _ _ _ (Path Box) (Target Empty) = True
  -- box to box is false in any cell type
  checkCellTypesToMove _ _ _ (Path Box)  (Path Box) = False
  checkCellTypesToMove _ _ _ (Target Box) (Target Box) = False
  checkCellTypesToMove _ _ _ (Target Box) (Path Box) = False
  checkCellTypesToMove _ _ _ (Path Box) (Target Box) = False
  -- person to box need to have some checkings recursively
  checkCellTypesToMove sm p trans (Path Player) (Target Box) = checkCellTypesToMoveHelper sm p trans
  checkCellTypesToMove sm p trans (Target Player) (Path Box) = checkCellTypesToMoveHelper sm p trans
  checkCellTypesToMove sm p trans (Target Player) (Target Box) = checkCellTypesToMoveHelper sm p trans
  checkCellTypesToMove sm p trans (Path Player) (Path Box) = checkCellTypesToMoveHelper sm p trans

  checkCellTypesToMoveHelper sm p trans = 
    let
      p'  = trans p
      ct  = (sm ! p)
      ct' = (sm ! p')
    in  
      checkCellTypesToMove sm p' trans ct ct' 
  

