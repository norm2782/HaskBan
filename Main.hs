module Main where
  import qualified Data.ByteString as BS
  import Control.Monad (liftM)
  import Control.Monad.State (runStateT)
  import HaskBan.Parser
  import HaskBan.Types 
  import qualified HaskBan as HB
  import Data.IntMap ((!))
  
  main :: IO ()
  main = do
    sokoMap <- readSokoMap
    (a, s) <- runSokobanMonad HB.main sokoMap
    return ()
        
  buildSokoBanInfo :: SokoMap -> SokobanInfo
  buildSokoBanInfo sm = SokobanInfo {
    currentLevel = 1,
    player = (0, 0),
    boxes = [],
    targets = [],
    sokoMap = sm 
  }

  readSokoMap :: IO SokobanInfo
  readSokoMap = (buildSokoBanInfo . (!0) . parseSokoMaps) `liftM` (BS.readFile "input.in")
