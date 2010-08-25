module HaskBan.Logic where
  
  import HaskBan.Types 
  import Data.Map ((!))

  updateList :: (Eq a) => (a -> a) -> a -> [a] -> [a]
  updateList _ _ [] = []
  updateList fn x (y:ys) 
    | x == y = ((fn x):ys)
    | otherwise = updateList fn x ys

  translateUp :: Translation
  translateUp (x, y)    = (x, y - 1)

  translateDown :: Translation
  translateDown (x, y)  = (x, y + 1)

  translateLeft :: Translation
  translateLeft (x, y)  = (x - 1, y)

  translateRight :: Translation
  translateRight (x, y) = (x + 1, y)
  
  isWall :: Point -> SokoMap -> Bool
  isWall = isCellType Wall

  isBox :: Point -> SokoMap -> Bool
  isBox p sm = (isCellType (Path Box) p sm) ||
               (isCellType (Target Box) p sm)

  isPath :: Point -> SokoMap -> Bool
  isPath = isCellType (Path Empty)

  isCellType :: CellType -> Point -> SokoMap -> Bool
  isCellType cType point sMap = (sMap ! point) == cType

  -- Verify if the player can move to the point that is provided.
  -- In case the new pointis a box, the next position needs to be
  -- checked as well. Hence, the original translation function is provided as well.
  canMoveTo :: SokoMap -> Point -> Translation -> Bool
  canMoveTo sMap point transl = (isPath point sMap) -- || 
                                -- (isBox  point sMap && not (isWall (transl point) sMap)) 


