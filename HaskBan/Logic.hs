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
  canMoveTo sMap point transl = (isPath   point sMap) ||
                                (isTarget point sMap) ||
                                (isBox    point sMap  && not (isWall trPt sMap)
                                                      && not (isBox  trPt sMap))
                                where trPt = transl point


