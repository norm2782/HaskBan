{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HaskBanPrinter where

  import HaskBanTypes (CellType(..), CellMatrix)
  import Data.List (intercalate, intersperse)

  showCellMatrix :: CellMatrix -> String
  showCellMatrix = intercalate "\n" . map (concat . (map show))
