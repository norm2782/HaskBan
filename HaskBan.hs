module HaskBan (mainAction) where
  
  import HaskBanTypes
  import HaskBanParser (runHaskBanParser)
  import HaskBanPrinter 
  import Control.Monad (mapM_)
  import qualified Data.ByteString as BS
  
  mainAction :: IO ()
  mainAction = BS.readFile "input.in" >>= \contents ->
               mapM_ (putStrLn . showCellMatrix) (runHaskBanParser contents)
  

