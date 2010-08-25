module HaskBan.Test.LogicTest where

  import Test.HUnit
  import Control.Monad

  import HaskBan.Test.TestHelper
  import HaskBan.Parser
  import HaskBan.Logic
  import HaskBan.Types

  --testIsPathWorks = TestCase (do
  --  sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")
  --  let works = isPath  
  --  )

  testCanMoveToWorksOnEmptyPaths = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")
    let playerCanMove = canMoveTo sokoMap (4,5) translateUp
    assertBool "User Player should move to an empty cell" playerCanMove
    )
  
  testCanMoveToWorksWithValidBox = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")
    let playerCanMove = canMoveTo sokoMap (4,5)  translateDown
    assertBool "User Player can move to a box that is followed by an empty space" playerCanMove
    )

  testCanMoveToFailsWithBoxFollowedByWall = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")
    let playerCanMove = canMoveTo sokoMap (4, 6) translateRight
    assertBool "User Player can move to a box that is followed by a wall" (not playerCanMove)
    )

  testCanMoveToFailsWithBoxFollowedByBox = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")
    let playerCanMove = canMoveTo sokoMap (4, 6) translateUp
    assertBool "User Player can move to a box that is followed by a box" (not playerCanMove)
    )

  haskBanLogicTestSuite = TestList [testCanMoveToWorksOnEmptyPaths,
                                    testCanMoveToWorksWithValidBox,
                                    testCanMoveToFailsWithBoxFollowedByWall,
                                    testCanMoveToFailsWithBoxFollowedByBox]
  
  

