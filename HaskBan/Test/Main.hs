module Main where

  import Test.HUnit
  import HaskBan.Test.LogicTest
  import HaskBan.Test.MonadTest
  import HaskBan.Test.ParserTest
  
  allSuite = TestList [TestLabel "LogicTestSuite" haskBanLogicTestSuite, 
                       TestLabel "MonadTestSuite" haskBanMonadTestSuite, 
                       TestLabel "ParserTestSuite" haskBanParserTestSuite]

  main :: IO ()
  main = do 
    runTestTT allSuite
    return ()
