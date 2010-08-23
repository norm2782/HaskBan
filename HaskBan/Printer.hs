{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HaskBan.Printer where

  import HaskBan.Types (CellType(..), CellMatrix)
  import Data.List (intercalate, intersperse)

  showCellMatrix :: CellMatrix -> String
  showCellMatrix = intercalate "\n" . map (concat . (map show))

    
    
