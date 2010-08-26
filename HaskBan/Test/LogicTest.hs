module HaskBan.Test.LogicTest where

  import Test.HUnit
  import Control.Monad

  import HaskBan.Test.TestHelper
  import HaskBan.Parser
  import HaskBan.Logic
  import HaskBan.Types

  getSokoMap = (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")

  testIsBoxWorks = TestCase (do
    sokoMap <- getSokoMap
    let plainBox  = isBox (3, 3) sokoMap
    let targetBox = isBox (1, 1) sokoMap
    assertBool "isBox works for box on path" plainBox
    assertBool "isBox works for box on target" targetBox
    )

  testIsPathWorks = TestCase (do
    sokoMap <- getSokoMap
    let worksOnPath = isPath (3, 5) sokoMap
    let failsOnWall = isPath (3, 8) sokoMap
    let failsOnBox  = isPath (3, 6) sokoMap
    assertBool "isPath works on empty path" worksOnPath
    assertBool "isPath fails on wall" (not failsOnWall)
    assertBool "isPath fails on box" (not failsOnBox)
    )
  
  testIsWallWorks = TestCase (do
    sokoMap <- getSokoMap
    let isAWall = isWall (0,0) sokoMap
    assertBool "Upper left corner is a wall" isAWall
    )

  testCanMoveToWorksOnEmptyPaths = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (3,5) translateUp
    assertBool "User Player can move to an empty cell" playerCanMove
    )
  
  testCanMoveToWorksWithValidBox = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (3,4)  translateUp
    assertBool "User Player can move to a box that is followed by an empty space" playerCanMove
    )

  testCanMoveToFailsWithBoxFollowedByWall = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (5,1) translateLeft
    assertBool "User Player can move to a box that is followed by a wall" (not playerCanMove)
    )

  testCanMoveToFailsWithBoxFollowedByBox = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (4, 6) translateUp
    assertBool "User Player can move to a box that is followed by a box" (not playerCanMove)
    )

  testCanMoveToFailsWithBoxFollowedByTwoBoxes = TestCase (do
    sokoMap <- getSokoMap
    let playerCanMove = canMoveTo sokoMap (5, 1) translateDown
    assertBool "User Player can move to a box that is followed by 2 boxes" (not playerCanMove)
    )

  haskBanLogicTestSuite = TestList [testIsBoxWorks,
                                    testIsPathWorks,
                                    testIsWallWorks,
                                    testCanMoveToWorksOnEmptyPaths,
                                    testCanMoveToWorksWithValidBox,
                                    testCanMoveToFailsWithBoxFollowedByWall,
                                    testCanMoveToFailsWithBoxFollowedByBox,
                                    testCanMoveToFailsWithBoxFollowedByTwoBoxes]

