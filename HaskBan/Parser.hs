{-# LANGUAGE NoMonomorphismRestriction #-}
-- | The Parser will interpret characters the following way:
-- 1) The test case starts with the string "Level #" where # is the number of the level
-- 2) The following lines will contain a set of characters that go the following way
--    # -> Wall
--    @ -> First position of the player
--    . -> Target position (pit)
--    $ -> Box that needs to be put on a pit
--    * -> Target position with a box on it 
--
--  3) The parsing will conclude when you get to the EOF or when the word END is parsed
--
module HaskBan.Parser (runHaskBanParser, validCellMatrix, cellMatrixToSokoMap) where

  import HaskBan.Types (CellType(..), CellMatrix, SokoMap)
  import Data.ByteString (ByteString)
  import Data.List (foldl')
  import qualified Data.Map as M
  import Text.Parsec hiding (many, optional)
  import Text.Parsec.ByteString
  import Control.Applicative

  -- | Basic Methods

  readInt :: String -> Int
  readInt = read

  -- | Custom Parsers (Parsec) 
  -- 
  parseInt = readInt <$> (many digit)
  parsePlayer = Player <$ char '@' 
  parseWall   = Wall <$ char '#' 
  parseBox    = Box <$ char '$'
  parsePath   = Path <$ char ' '
  parseTarget = Target Nothing <$ char '.'
  parseRockOnTarget = Target (Just Box) <$ char '*' 

  parseCellType    = choice [parseWall, parseBox, parsePath, parseTarget,
                             parsePlayer, parseRockOnTarget]
  parseCellTypeRow = many1 parseCellType <* char '\n'
  parseCellMatrix  = string "Level" *> spaces *> parseInt *> spaces *> (many parseCellTypeRow) <* spaces
  parseEndSection  = string "END"
  parseHaskBan     = (many parseCellMatrix) <* optional parseEndSection

  runHaskBanParser :: ByteString -> [CellMatrix]
  runHaskBanParser input = 
    case parse parseHaskBan "()" input of
      Left e -> error (show e)
      Right celltypes -> celltypes

  -- | CellMatrix methods 
  --
  validCellMatrix :: CellMatrix -> Maybe CellMatrix 
  validCellMatrix [] = Just []
  validCellMatrix matrix@(row:rows) = if rowsHaveSameLength then (Just matrix) else Nothing
    where
      rowLength = length row
      rowsHaveSameLength = all id $ map ((rowLength==) . length) rows

  -- |  This method is assuming it is recieving a valid CellMatrix
  --
  cellMatrixToSokoMap :: CellMatrix -> SokoMap
  cellMatrixToSokoMap cellMatrix = snd (foldl' columnHelper (0, M.empty) cellMatrix)
    where
      columnHelper (i, sokoMap) cellTypeRow = 
        let (_, sokoMap') = foldl' rowHelper ((i, 0), sokoMap) cellTypeRow
        in (i + 1, sokoMap') 
      rowHelper ((i, j), sokoMap) cellType = 
        let sokoMap' =  M.insert (i, j) cellType sokoMap
        in ((i, j + 1), sokoMap') 

  
