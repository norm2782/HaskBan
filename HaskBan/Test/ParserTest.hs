module HaskBan.Test.ParserTest where

  import Test.HUnit
  import Control.Monad (liftM)
  import HaskBan.Parser (runHaskBanParser, validCellMatrix)
  import HaskBan.Test.TestHelper 

  -- | test methods
  --
  testCellMatrixIsInvalidWhenRowsHaveDifferentLength = TestCase (do
    -- In the fixture, matrix #2 is invalid
    cellMatrixes <- runHaskBanParser `liftM` fixture "CellMatrixFixture.txt"
    let cellMatrix = cellMatrixes !! 1
    let expected = Nothing
    let actual = validCellMatrix cellMatrix
    assertEqual "CellMatrix is invalid when rows have different length" actual Nothing)

  testCellMatrixIsValidWhenRowsHaveSameLength = TestCase (do
    cellMatrixes <- runHaskBanParser `liftM` fixture "CellMatrixFixture.txt"
    let cellMatrix = cellMatrixes !! 0
    let expected = Just cellMatrix
    let actual = validCellMatrix cellMatrix
    assertEqual "CellMatrix is valid when rows have same length" expected actual
    )
    
  
  haskBanParserTestSuite = TestList [testCellMatrixIsInvalidWhenRowsHaveDifferentLength,
                                     testCellMatrixIsValidWhenRowsHaveSameLength]


