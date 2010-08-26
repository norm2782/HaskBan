module HaskBan.Test.MonadTest where

  import Test.HUnit
  import Data.Map ((!))
  import Control.Monad

  import HaskBan.Test.TestHelper
  import HaskBan.Types
  import HaskBan.Monad
  import HaskBan.Logic
  import HaskBan.Parser

  getSokoMap str = (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture str)
  assertCellTypeContains ct ic = assertBool ("CellType " ++ (show ct) ++  " contains correct inner type") 
                                            (inner ct == ic)

  testMovePlayerFailsWithBoxesInARow = TestCase $ do
    sm <- getSokoMap "SokoMapMovePlayerFixture.txt"
    assertCellTypeContains (sm ! (4, 1)) Player
    assertCellTypeContains (sm ! (5, 1)) Box
    assertCellTypeContains (sm ! (6, 1)) Box
    assertCellTypeContains (sm ! (7, 1)) Box
    let sokobanInfo = getSokobanInfo sm
    (_, sokobanInfo') <- runSokobanMonad (movePlayer translateDown) sokobanInfo 
    let sm' = sokoMap sokobanInfo'
    assertCellTypeContains (sm ! (4, 1)) Player
    assertCellTypeContains (sm ! (5, 1)) Box
    assertCellTypeContains (sm ! (6, 1)) Box
    assertCellTypeContains (sm ! (7, 1)) Box
    

  testMoveBoxFollowedBy2BoxesWontDoShit = TestCase $ do
    sm <- getSokoMap "SokoMapLogicFixture.txt"
    assertCellTypeContains (sm ! (5, 1)) Box
    assertCellTypeContains (sm ! (6, 1)) Box
    assertCellTypeContains (sm ! (7, 1)) Box
    let sokobanInfo = getSokobanInfo sm
    (_, sokobanInfo') <- runSokobanMonad (moveBox (5, 1) translateDown) sokobanInfo 
    let sm' = sokoMap sokobanInfo'
    assertCellTypeContains (sm ! (5, 1)) Box
    assertCellTypeContains (sm ! (6, 1)) Box
    assertCellTypeContains (sm ! (7, 1)) Box

  -- Initially:
  -- (3,3) holds a Box Path
  -- (2,3) holds an Empty Path
  testMoveBoxUpdateSokoMap = TestCase $ do
    sm <- getSokoMap "SokoMapLogicFixture.txt"
    assertCellTypeContains (sm ! (2,3)) Empty
    assertCellTypeContains (sm ! (3,3)) Box
    let sokobanInfo = getSokobanInfo sm
    (_, sokobanInfo') <- runSokobanMonad (moveBox (3, 3) translateUp) sokobanInfo 
    let sm' = sokoMap sokobanInfo'
    assertCellTypeContains (sm' ! (2,3)) Box
    assertCellTypeContains (sm' ! (3,3)) Empty


  haskBanMonadTestSuite = TestList [testMovePlayerFailsWithBoxesInARow, 
                                    testMoveBoxUpdateSokoMap, 
                                    testMoveBoxFollowedBy2BoxesWontDoShit]

