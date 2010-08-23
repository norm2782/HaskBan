module HaskBan.Test.ParserTest where

  import Test.HUnit
  import Control.Monad (liftM)
  import HaskBan.Parser (runHaskBanParser, validCellMatrix)
  import HaskBan.Test.TestHelper 

  -- | test methods
  --
  testCellMatrixIsInvalidWhenRowsHaveSameLength = TestCase (do
    -- In the fixture, matrix #2 is invalid
    cellMatrixes <- runHaskBanParser `liftM` fixture "CellMatrixFixture.txt"
    let expected = Nothing
    let actual = validCellMatrix (cellMatrixes !! 1)
    assertEqual "CellMatrix is invalid when rows have different length" actual Nothing)

  testCellMatrixIsValidWhenRowsHaveSameLength = TestCase (do
    cellMatrixes <- runHaskBanParser `liftM` fixture "CellMatrixFixture.txt"
    let expected = Just (cellMatrixes !! 0)
    let actual = validCellMatrix (cellMatrixes !! 0)
    assertEqual "CellMatrix is valid when rows have same length" expected actual
    )
    
  
  haskBanParserTestSuite = TestList [testCellMatrixIsInvalidWhenRowsHaveSameLength,
                                     testCellMatrixIsValidWhenRowsHaveSameLength]


