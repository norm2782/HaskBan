module Main where
  import qualified Data.ByteString as BS
  import Control.Monad (liftM)
  import Control.Monad.State (runStateT)
  import HaskBan.Monad (runSokobanMonad)
  import HaskBan.Parser
  import HaskBan.Types 
  import qualified HaskBan as HB
  import Data.IntMap ((!))
  
  main :: IO ()
  main = do
    sokobanInfo <- readSokobanInfo
    (a, s) <- runSokobanMonad HB.main sokobanInfo
    return ()
        
  readSokobanInfo :: IO SokobanInfo
  readSokobanInfo = (getSokobanInfo . (!0) . parseSokoMaps) `liftM` (BS.readFile "input.in")
