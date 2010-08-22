{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HaskBanPrinter where

  import HaskBanTypes (CellType(..), CellMatrix)
  import Data.List (intercalate, intersperse)

  instance Show CellType where
    show Wall = "#"
    show Player = "λ"
    show Box = "$"
    show Path = " "
    show (Target (Nothing)) = "."
    show (Target (Just _)) = "*"
    show Empty = " "

  showCellMatrix :: CellMatrix -> String
  showCellMatrix = intercalate "\n" . map (concat . (map show))

    
    
