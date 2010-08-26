module HaskBan.Test.ParserTest where

  import Test.HUnit
  import qualified Data.Map as M
  import Control.Monad (liftM)
  import HaskBan.Types (CellType(..), InnerCell(..))
  import HaskBan.Parser (runHaskBanParser, validCellMatrix, cellMatrixToSokoMap)
  import HaskBan.Test.TestHelper (fixture) 

  -- | test methods
  --
  testCellMatrixIsInvalidWhenRowsHaveDifferentLength = TestCase (do
    -- In the fixture, matrix #2 is invalid
    cellMatrixes <- getCellMatrixFixture
    let cellMatrix = cellMatrixes !! 1
    let expected   = Nothing
    let actual     = validCellMatrix cellMatrix
    assertEqual "CellMatrix is invalid when rows have different length" actual Nothing)

  testCellMatrixIsValidWhenRowsHaveSameLength = TestCase (do
    cellMatrixes <- getCellMatrixFixture
    let cellMatrix = cellMatrixes !! 0
    let expected   = Just cellMatrix
    let actual     = validCellMatrix cellMatrix
    assertEqual "CellMatrix is valid when rows have same length" expected actual
    )

  testSokoMapCreationKeepsBoundaries = TestCase (do
    ~cellMatrix@(r:rs) <- getSokoMapFixture
    let sokoMap = cellMatrixToSokoMap cellMatrix
    assertEqual "SokoMap has same boundories as the CellMatrix" (length (M.keys sokoMap)) (length cellMatrix * length r)
    )

  testSokoMapCreationHasPlayer = TestCase (do
    cellMatrix <- getSokoMapFixture
    let sokoMap = cellMatrixToSokoMap cellMatrix
    -- (4, 7) should be the player
    case M.lookup (4, 7) sokoMap of
      Just (Path Player) -> assertBool "The player is on the right position" True
      x -> putStrLn "" >> putStrLn (show x) >> putStrLn "" >> assertBool "Player not Matching" False
    )
    
  getSokoMapFixture    = (head . runHaskBanParser) `liftM` fixture "SokoMapCreationFixture.txt" 
  getCellMatrixFixture = runHaskBanParser `liftM` fixture "ValidCellMatrixFixture.txt"
 
  haskBanParserTestSuite = TestList [testCellMatrixIsInvalidWhenRowsHaveDifferentLength,
                                     testCellMatrixIsValidWhenRowsHaveSameLength,
                                     testSokoMapCreationKeepsBoundaries,
                                     testSokoMapCreationHasPlayer]


