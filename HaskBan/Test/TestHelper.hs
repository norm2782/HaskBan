module HaskBan.Test.TestHelper (fixture) where
  
  import qualified Data.ByteString as BS
  import System.Directory
  
  fixture :: FilePath -> IO BS.ByteString
  fixture fileName = do
    fp <- fixturesPath
    let fixtureFilePath = concat [fp, ('/':fileName)]
    BS.readFile fixtureFilePath
  
  fixturesPath :: IO FilePath
  fixturesPath = do
    path <- getCurrentDirectory
    return $ concat [path, "/HaskBan/Test/Fixtures"]

