module HaskBan.Test.LogicTest where

  import Test.HUnit
  import Control.Monad

  import HaskBan.Test.TestHelper
  import HaskBan.Parser
  import HaskBan.Logic
  import HaskBan.Types

  testIsPathWorks = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")
    putStrLn (show sokoMap)
    let worksOnPath = isPath (3, 5) sokoMap
    let failsOnWall = isPath (3, 8) sokoMap
    let failsOnBox  = isPath (3, 6) sokoMap
    assertBool "isPath works on empty path" worksOnPath
    assertBool "isPath fails on wall" (not failsOnWall)
    assertBool "isPath fails on box" (not failsOnBox)
    )

  testCanMoveToWorksOnEmptyPaths = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")
    let playerCanMove = canMoveTo sokoMap (3,5) translateUp
    assertBool "User Player can move to an empty cell" playerCanMove
    )
  
  testCanMoveToWorksWithValidBox = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")
    let playerCanMove = canMoveTo sokoMap (3,4)  translateUp
    assertBool "User Player can move to a box that is followed by an empty space" playerCanMove
    )

  testCanMoveToFailsWithBoxFollowedByWall = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")
    let playerCanMove = canMoveTo sokoMap (4, 6) translateRight
    assertBool "User Player can move to a box that is followed by a wall" (not playerCanMove)
    )

  testCanMoveToFailsWithBoxFollowedByBox = TestCase (do
    sokoMap <- (cellMatrixToSokoMap . head . runHaskBanParser) `liftM` (fixture "SokoMapLogicFixture.txt")
    let playerCanMove = canMoveTo sokoMap (4, 6) translateUp
    assertBool "User Player can move to a box that is followed by a box" (not playerCanMove)
    )

  haskBanLogicTestSuite = TestList [testIsPathWorks,
                                    testCanMoveToWorksOnEmptyPaths,
                                    testCanMoveToWorksWithValidBox,
                                    testCanMoveToFailsWithBoxFollowedByWall,
                                    testCanMoveToFailsWithBoxFollowedByBox]
  
  

