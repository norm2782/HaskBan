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

  parsePlayer = char '@' *> pure Player
  parseWall = char '#' *> pure Wall
  parseBox  = char '$' *> pure Box
  parsePath = char ' ' *> pure Path
  parseTarget = char '.' *> pure (Target Nothing)
  parseRockOnTarget = char '*' *> pure (Target (Just Box))

  parseCellType = choice [parseWall, parseBox, parsePath, parseTarget, parsePlayer, parseRockOnTarget]
  parseCellTypeRow = many parseCellType <* char '\n'
  parseCellMatrix = string "Level " *> parseInt *> spaces *> (many parseCellTypeRow)
  parseEndSection = spaces *> string "END"
  parseHaskBan = many parseCellMatrix <* optional parseEndSection

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

  
