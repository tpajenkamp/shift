{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, InstanceSigs #-}
-----------------------------------------------------------------------------
--
-- Module      :  ShiftGame.GTKScenarioView
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | GTK based view for game model
--
-----------------------------------------------------------------------------

module ShiftGame.GTKScenarioView where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy
import           Data.IORef
import           Graphics.UI.Gtk

import ShiftGame.Scenario
import ShiftGame.ScenarioController

data ControlSettings sc = ControlSettings { keysLeft   :: [KeyVal] -- ^ keys (alternatives) to trigger a "left" movement
                                       , keysRight  :: [KeyVal] -- ^ keys (alternatives) to trigger a "right" movement
                                       , keysUp     :: [KeyVal] -- ^ keys (alternatives) to trigger an "up" movement
                                       , keysDown   :: [KeyVal] -- ^ keys (alternatives) to trigger a "down" movement
                                       , keysQuit   :: [KeyVal] -- ^ keys (alternatives) to exit the game
                                       , keysReset  :: [KeyVal] -- ^ keys (alternatives) to restart the level
                                       , keysUndo   :: [KeyVal] -- ^ keys (alternatives) to undo a single step
                                       , keysRedo   :: [KeyVal] -- ^ keys (alternatives) to redo a single step
                                       , initialScenario :: ScenarioState sc -- ^ currently loaded scenario
                                       } deriving (Eq, Show, Read)


data TextViewUpdateListener = TextViewUpdateListener TextBuffer

instance UpdateListener TextViewUpdateListener IO MatrixScenario where
  notifyUpdate :: TextViewUpdateListener -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO ()
  notifyUpdate (TextViewUpdateListener tBuffer) _ = do
      scState <- ask -- todo: player position
      let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
      (lift . textBufferSetByteString tBuffer) levelStrWithPlayer
  notifyNew :: TextViewUpdateListener -> ReaderT (ScenarioState MatrixScenario) IO ()
  notifyNew (TextViewUpdateListener tBuffer) = do
      scState <- ask -- todo: player position
      let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
      (lift . textBufferSetByteString tBuffer) levelStrWithPlayer
  notifyWin :: TextViewUpdateListener -> ReaderT (ScenarioState MatrixScenario) IO ()
  notifyWin (TextViewUpdateListener tBuffer) = do lift $ putStrLn "you win!"



setInitialScenario :: Scenario sc => ControlSettings sc -> ScenarioState sc -> ControlSettings sc
setInitialScenario cs s = cs { initialScenario = s }

createTextViewLink :: TextBuffer -> TextViewUpdateListener
createTextViewLink tBuffer = TextViewUpdateListener tBuffer




-- implementation detaiol: GTK event handling does not (easily) allow mixing the Event monad with e. g. State or Reader
-- that is the reason why an IORef is used.
-- | Processes keyboard events and determines the resulting player action.
keyboardHandler :: (Scenario sc, ScenarioController ctrl sc IO) => IORef (ControlSettings sc, ctrl) -> EventM EKey Bool
keyboardHandler ref = do (ctrlSettings, ctrlState) <- (lift . readIORef) ref
                         keyV <- eventKeyVal
                         -- test to quit game
                         if (keyV `elem` keysQuit ctrlSettings)
                         then lift mainQuit >> return True
                         else if (keyV `elem` keysReset ctrlSettings)
                         then do (_, newState) <- lift $ runStateT (setScenario (initialScenario ctrlSettings)) ctrlState
                                 lift $ putStrLn "level reset"
                                 (lift . writeIORef ref) (ctrlSettings, newState)
                                 return True
                         else if (keyV `elem` keysUndo ctrlSettings)
                         then do (err, newState) <- lift $ runStateT undoAction ctrlState
                                 lift $ putStrLn $ maybe "undo" ((++) "undo: " . show) err
                                 (lift . writeIORef ref) (ctrlSettings, newState)
                                 return True
                         else if (keyV `elem` keysRedo ctrlSettings)
                         then do (err, newState) <- lift $ runStateT redoAction ctrlState
                                 lift $ putStrLn $ maybe "redo" ((++) "redo: " . show) err
                                 (lift . writeIORef ref) (ctrlSettings, newState)
                                 return True   
                         else do
                             let mbPlayerAction = if keyV `elem` keysLeft ctrlSettings  then Just MLeft
                                             else if keyV `elem` keysRight ctrlSettings then Just MRight
                                             else if keyV `elem` keysUp ctrlSettings    then Just MUp
                                             else if keyV `elem` keysDown ctrlSettings  then Just MDown
                                             else Nothing
                              -- run controller
                             case mbPlayerAction of
                                  Nothing -> do lift . putStrLn $ "unknown key command: (" ++ show keyV ++ ") " ++ (show . keyName) keyV
                                                return False
                                  Just action -> do (lift . putStrLn . show) action
                                                    (denyReason, newState) <- lift $ runStateT (runPlayerMove action) ctrlState
                                                    case denyReason of
                                                         Just r -> lift $ putStrLn $ show r                             -- failure
                                                         Nothing -> (lift . writeIORef ref) (ctrlSettings, newState)    -- success
                                                    return True


                         
                         
