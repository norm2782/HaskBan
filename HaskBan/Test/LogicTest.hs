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

  testIsWallWorks = TestCase (do
    sokoMap <- getSokoMap
    let isAWall = isWall (0,0) sokoMap
    assertBool "Upper left corner is a wall" isAWall
    )

  testCanMoveToWorksOnEmptyPaths = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (4,5) translateUp
    assertBool "User Player should move to an empty cell" playerCanMove
    )
  
  testCanMoveToWorksWithValidBox = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (4,5)  translateDown
    assertBool "User Player can move to a box that is followed by an empty space" playerCanMove
    )

  testCanMoveToFailsWithBoxFollowedByWall = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (4, 6) translateRight
    assertBool "User Player can move to a box that is followed by a wall" (not playerCanMove)
    )

  testCanMoveToFailsWithBoxFollowedByBox = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (4, 6) translateUp
    assertBool "User Player can move to a box that is followed by a box" (not playerCanMove)
    )

  getSokoMap = (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapCreationFixture.txt")

  haskBanLogicTestSuite = TestList [testIsWallWorks,
                                    testCanMoveToWorksOnEmptyPaths,
                                    testCanMoveToWorksWithValidBox,
                                    testCanMoveToFailsWithBoxFollowedByWall,
                                    testCanMoveToFailsWithBoxFollowedByBox]
  
  

