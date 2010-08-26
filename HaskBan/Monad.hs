{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module HaskBan.Monad where

  import UI.HSCurses.Curses
  -- import System.IO.Unsafe (unsafePerformIO)

  import HaskBan.Types (Point, 
                        SokoMap, 
                        InnerCell(..),
                        CellType(..), 
                        SokobanInfo(..), 
                        Translation)
  import HaskBan.Printer (showSokoMap)
  import HaskBan.Logic (isBox, updateList, canMoveTo)
  import Control.Monad.State
  import qualified Data.Map as M
  import Data.List (sort)
  import Data.Map ((!))

  newtype SokobanMonad a = SokobanMonad (StateT SokobanInfo IO a)
                           deriving (Monad, MonadIO, MonadState SokobanInfo)
  
  -- | With this method you will startup the SokobanMonad
  runSokobanMonad :: SokobanMonad a -> SokobanInfo -> IO (a, SokobanInfo)
  runSokobanMonad (SokobanMonad stateT) initialState = runStateT stateT initialState
  
  -- | Accessors of the SokobanMonad Attributes
  --

  isGameFinished :: (MonadState SokobanInfo) m => m Bool
  isGameFinished = do
    bs <- sort `liftM` getBoxesPositions
    ts <- sort `liftM` getTargetPositions
    return $ bs == ts

  getPlayerPosition :: (MonadState SokobanInfo) m => m Point
  getPlayerPosition = player `liftM` get
  
  putPlayerPosition :: (MonadState SokobanInfo) m => Point -> m ()
  putPlayerPosition position = get >>= \state -> put (state {player = position})

  updatePlayerPosition :: (MonadState SokobanInfo) m => (Point -> Point) -> m ()
  updatePlayerPosition trans = getPlayerPosition >>= putPlayerPosition . trans

  getTargetPositions :: (MonadState SokobanInfo) m => m [Point]
  getTargetPositions = targets `liftM` get

  getNumberOfSteps :: (MonadState SokobanInfo) m => m Int
  getNumberOfSteps = numSteps `liftM` get

  putNumberOfSteps :: (MonadState SokobanInfo) m => Int -> m ()
  putNumberOfSteps steps = get >>= \state -> put (state { numSteps = steps })

  incrNumberOfSteps :: (MonadState SokobanInfo) m => m ()
  incrNumberOfSteps = getNumberOfSteps >>= putNumberOfSteps . (+1)

  getBoxesPositions :: (MonadState SokobanInfo) m => m [Point]
  getBoxesPositions = boxes `liftM` get

  putBoxesPositions :: (MonadState SokobanInfo) m => [Point] -> m ()
  putBoxesPositions bs = get >>= \state -> put (state { boxes = bs })

  updateBoxPosition :: (MonadState SokobanInfo) m => (Point -> Point) -> Point -> m () 
  updateBoxPosition trans box = putBoxesPositions . updateList trans box =<< getBoxesPositions 

  getMap :: (MonadState SokobanInfo) m => m SokoMap
  getMap = sokoMap `liftM` get 

  putMap :: (MonadState SokobanInfo) m => SokoMap -> m ()
  putMap map = get >>= \state -> put (state {sokoMap = map})

  updateMap :: (MonadState SokobanInfo) m => (SokoMap -> SokoMap) -> m ()
  updateMap fn = putMap . fn =<< getMap

  swapCellType :: Point -> Point -> SokoMap -> SokoMap
  swapCellType p1 p2 sm = sm''
    where
      ic1  = inner (sm ! p1)
      ic2  = inner (sm ! p2)
      sm'  = updateCellType sm p1 ic2
      sm'' = updateCellType sm' p2 ic1
      
  -- | Helper methods to update the state inside the monad
  --
  updateCellType :: SokoMap -> Point -> InnerCell -> SokoMap
  updateCellType sm p innerCell = M.update updateInnerCell p sm
    where
      updateInnerCell (Path _)   = Just $ Path innerCell
      updateInnerCell (Target _) = Just $ Target innerCell
      updateInnerCell Wall       = Just $ Wall

  -- | We are going to swap the current position (where the box is) 
  -- with the position given after the translation
  moveBox :: (MonadState SokobanInfo) m => Point -> Translation -> m ()
  moveBox p trans = do
    sm <- getMap
    let p' = trans p
    updateMap (swapCellType p p')
    updateBoxPosition trans p

  -- | SokobanMonad action that cheks the current state and moves
  -- the player internally on the monad
  -- We need to:
  -- 1. Update the SokobanInfo
  -- 2. Update the SokoMap
  -- movePlayer :: Translation -> SokobanMonad ()
  movePlayer :: (MonadState SokobanInfo) m => Translation -> m Bool
  movePlayer trans = do
    sm   <- getMap
    ppos <- getPlayerPosition
    let ppos' = trans ppos
    when (canMoveTo sm ppos' trans) $ do
      incrNumberOfSteps
      when (isBox ppos' sm) $ do
        moveBox ppos' trans
      -- we have updated the SokobanInfo
      updateMap (swapCellType ppos ppos')
      updatePlayerPosition trans
    isGameFinished
      

