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
createTextViewLink = UpdateListener . textViewListenerFunction

textViewListenerFunction :: TextBuffer -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO ()
textViewListenerFunction tBuffer _ = do scState <- ask -- todo: player position
                                        let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
                                        (lift . textBufferSetByteString tBuffer) levelStrWithPlayer


-- implementation detaiol: GTK event handling does not (easily) allow mixing the Event monad with e. g. State or Reader
-- that is the reason why an IORef is used.

keyboardHandler :: IORef (ControlSettings, ControllerState m sc) -> EventM EKey Bool
keyboardHandler ref = do (ctrlSettings, ctrlState) <- (lift . readIORef) ref
                         key <- eventKeyVal
                         -- left
                         if key `elem` keysLeft ctrlSettings
                         then (lift . putStrLn . show) MLeft >> return True
                         -- right
                         else if key `elem` keysRight ctrlSettings
                         then (lift . putStrLn . show) MRight >> return True
                         -- up
                         else if key `elem` keysUp ctrlSettings
                         then (lift . putStrLn . show) MUp >> return True
                         -- down
                         else if key `elem` keysDown ctrlSettings
                         then (lift . putStrLn . show) MDown >> return True
                         -- unknown
                         else (lift . putStrLn $ "unknown key command: (" ++ show key ++ ") " ++ (show . keyName) key) >> return False