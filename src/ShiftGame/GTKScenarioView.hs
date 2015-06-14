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

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B
import           Graphics.UI.Gtk

import ShiftGame.Scenario
import ShiftGame.ScenarioController

createTextViewLink :: TextBuffer -> UpdateListener MatrixScenario
createTextViewLink = UpdateListener . textViewListenerFunction

textViewListenerFunction :: TextBuffer -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO ()
textViewListenerFunction tBuffer _ = do scState <- ask -- todo: player position
                                        let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
                                        (liftIO . textBufferSetByteString tBuffer) levelStrWithPlayer

