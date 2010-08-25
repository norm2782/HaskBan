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
module HaskBan.Parser (parseSokoMaps, runHaskBanParser, validCellMatrix, cellMatrixToSokoMap) where

  import HaskBan.Types (CellType(..), CellMatrix, SokoMap, SokoMaps)
  import Data.ByteString (ByteString)
  import Data.Maybe (catMaybes)
  import Data.List (foldl')
  import qualified Data.IntMap as IM
  import qualified Data.Map as M
  import Text.Parsec hiding (many, optional)
  import Text.Parsec.ByteString
  import Control.Applicative

  -- | Basic Methods

  readInt :: String -> Int
  readInt = read

  -- | Custom Parsers (Parsec) 
  -- 
  pInt    = readInt <$> (many digit)
  pPlayer = Player  <$  char '@' 
  pWall   = Wall    <$  char '#' 
  pBox    = Box     <$  char '$'
  pPath   = Path    <$  char ' '
  pTarget = Target Nothing <$ char '.'
  pRockOnTarget = Target (Just Box) <$ char '*' 

  pCellType    = choice [pWall, pBox, pPath, pTarget,
                             pPlayer, pRockOnTarget]
  pCellTypeRow = many1 pCellType <* char '\n'
  pCellMatrix  = string "Level" *> spaces *> pInt *> spaces *> (many pCellTypeRow) <* spaces
  pEndSection  = string "END"
  pHaskBan     = (many pCellMatrix) <* optional pEndSection

  runHaskBanParser :: ByteString -> [CellMatrix]
  runHaskBanParser input = 
    case parse pHaskBan "()" input of
      Left e -> error (show e)
      Right cells -> (catMaybes . map validCellMatrix) cells


  -- | Main Method
  parseSokoMaps :: ByteString -> SokoMaps
  parseSokoMaps bs = IM.fromList . zip [0..] . map cellMatrixToSokoMap . runHaskBanParser $ bs

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

