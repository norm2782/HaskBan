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
-- parseSokoMap
module HaskBan.Parser (runHaskBanParser, validCellMatrix) where

  import HaskBan.Types (CellType(..), CellMatrix, SokoMap)
  import Data.ByteString (ByteString)
  import Text.Parsec hiding (many, optional)
  import Text.Parsec.ByteString
  import Control.Applicative

  readInt :: String -> Int
  readInt = read

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

  validCellMatrix :: CellMatrix -> Maybe CellMatrix 
  validCellMatrix [] = Just []
  validCellMatrix matrix@(row:rows) = if rowsHaveSameLength then (Just matrix) else Nothing
    where
      rowLength = length row
      rowsHaveSameLength = all id $ map ((rowLength==) . length) rows

  {--
  cellTypeMatrixToSokoMap :: [[CellType]] -> SokoMap
  cellTypeMatrixToSokoMap xs = helper 0 xs
    where
      helper i xs
  --}

  
