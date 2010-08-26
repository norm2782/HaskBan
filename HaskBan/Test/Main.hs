module Main where

  import Test.HUnit
  import HaskBan.Test.LogicTest
  import HaskBan.Test.MonadTest
  import HaskBan.Test.ParserTest
  
  allSuite = TestList [haskBanLogicTestSuite, 
                       haskBanMonadTestSuite, 
                       haskBanParserTestSuite]

  main :: IO ()
  main = do 
    runTestTT allSuite
    return ()
