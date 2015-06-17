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

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy
import qualified Data.ByteString.Char8 as B
import           Data.IORef
import           Graphics.UI.Gtk

import ShiftGame.Scenario
import ShiftGame.ScenarioController

data ControlSettings = ControlSettings { keysLeft   :: [KeyVal] -- ^ keys (alternatives) to trigger a "left" movement
                                       , keysRight  :: [KeyVal] -- ^ keys (alternatives) to trigger a "right" movement
                                       , keysUp     :: [KeyVal] -- ^ keys (alternatives) to trigger an "up" movement
                                       , keysDown   :: [KeyVal] -- ^ keys (alternatives) to trigger a "down" movement
                                       } deriving (Eq, Show, Read)


createTextViewLink :: TextBuffer -> UpdateListener IO MatrixScenario
createTextViewLink tBuffer = UpdateListener (textViewUpdateFunction tBuffer) (textViewCreateFunction tBuffer) (textViewWinFunction tBuffer)

textViewUpdateFunction :: TextBuffer -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO ()
textViewUpdateFunction tBuffer _ = do scState <- ask -- todo: player position
                                      let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
                                      (lift . textBufferSetByteString tBuffer) levelStrWithPlayer

textViewCreateFunction :: TextBuffer -> ReaderT (ScenarioState MatrixScenario) IO ()
textViewCreateFunction tBuffer = do scState <- ask -- todo: player position
                                    let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
                                    (lift . textBufferSetByteString tBuffer) levelStrWithPlayer

textViewWinFunction :: TextBuffer -> ReaderT (ScenarioState MatrixScenario) IO ()
textViewWinFunction _ = do lift $ putStrLn "you win!"


-- implementation detaiol: GTK event handling does not (easily) allow mixing the Event monad with e. g. State or Reader
-- that is the reason why an IORef is used.
-- | Processes keyboard events and determines the resulting player action.
keyboardHandler :: Scenario sc => IORef (ControlSettings, ControllerState IO sc) -> EventM EKey Bool
keyboardHandler ref = do (ctrlSettings, ctrlState) <- (lift . readIORef) ref
                         keyV <- eventKeyVal
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


                         
                         