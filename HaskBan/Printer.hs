{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HaskBan.Printer (showSokoMap) where

  import HaskBan.Types (CellType(..), CellMatrix, SokoMap, Point)
  import Data.Map ((!))
  import qualified Data.Map as M
  import Data.List (intercalate, intersperse, foldl')


  showSokoMap :: SokoMap -> String
  showSokoMap sokoMap = reverse $ intercalate "\n" $ map rowFn $ sokoMapKeysMatrix sokoMap
    where
      rowFn ps = concat $ map cellFn ps
      cellFn p = 
        case (M.lookup p sokoMap) of
          Just cell -> show cell
          Nothing -> ""

  sokoMapKeysMatrix :: SokoMap -> [[Point]]
  sokoMapKeysMatrix sokoMap = snd (foldl' helper (0, [[]]) (M.keys sokoMap))
    where 
      helper :: (Int, [[Point]]) -> Point -> (Int, [[Point]])
      helper (i, list@(l:ls)) k 
        | (fst k) == i = (i, ((k:l):ls))
        | otherwise = (i + 1, ([k]:list))
      
  
  showCellMatrix :: CellMatrix -> String
  showCellMatrix = intercalate "\n" . map (concat . (map show))
