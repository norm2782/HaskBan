module HaskBan.Test.LogicTest where

  import Test.HUnit
  import Control.Monad

  import HaskBan.Test.TestHelper
  import HaskBan.Parser
  import HaskBan.Logic
  import HaskBan.Types

  testCanMoveToWorksOnEmptyPaths = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")
    let playerCanMove = canMoveTo sokoMap (3,7) translateUp
    assertBool "User Player should move to an empty cell" playerCanMove
    )

  haskBanLogicTestSuite = TestList [testCanMoveToWorksOnEmptyPaths]
  
  

