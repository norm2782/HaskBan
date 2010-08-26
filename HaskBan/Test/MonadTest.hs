module HaskBan.Test.MonadTest where

  import Test.HUnit
  import Data.Map ((!))
  import Control.Monad

  import HaskBan.Test.TestHelper
  import HaskBan.Types
  import HaskBan.Monad
  import HaskBan.Logic
  import HaskBan.Parser

  getSokoMap = (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")
  assertCellTypeContains ct ic = assertBool ("CellType " ++ (show ct) ++  " contains correct inner type") 
                                            (inner ct == ic)

  -- Initially:
  -- (3,3) holds a Box Path
  -- (2,3) holds an Empty Path
  testMoveBoxUpdateSokoMap = TestCase $ do
    sm <- getSokoMap
    putStrLn ""
    putStrLn (show sm)
    assertCellTypeContains (sm ! (2,3)) Empty
    assertCellTypeContains (sm ! (3,3)) Box
    let sokobanInfo = getSokobanInfo sm
    (_, sokobanInfo') <- runSokobanMonad (moveBox (3, 3) translateUp) sokobanInfo 
    putStrLn ""
    let sm' = sokoMap sokobanInfo'
    putStrLn (show sm')
    assertCellTypeContains (sm' ! (2,3)) Box
    assertCellTypeContains (sm' ! (3,3)) Empty


  haskBanMonadTestSuite = TestList [testMoveBoxUpdateSokoMap]
