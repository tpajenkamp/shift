{-# LANGUAGE FlexibleContexts #-}
module ShiftGame.GtkShiftIO where

import           Control.Concurrent
import           Control.Exception (evaluate)
import           Control.Lens
import           Control.Monad.Trans.State.Lazy
import           Graphics.UI.Gtk hiding (get, set, rectangle)
import           System.Glib.UTFString

import ShiftGame.Helpers
import ShiftGame.GtkScenarioView
import ShiftGame.Scenario
import ShiftGame.ScenarioController
import ShiftGame.ShiftIO

{-
Gtk Dialog to choose level file
-}

-- | Asks the user to select a file. Only call this function from a thread that owns the Gtk lock!!!
showSelectScenarioDialog :: GlibFilePath fp => IO (Maybe fp)
showSelectScenarioDialog = do
   chooser <- fileChooserDialogNew (Just "Select level...") Nothing FileChooserActionOpen [("_Cancel", ResponseCancel), ("_Open", ResponseAccept)]
   fileChooserSetSelectMultiple chooser False
   dlgResponse <- dialogRun chooser
   widgetHide chooser
   returnVal <- if dlgResponse == ResponseAccept
                  then fileChooserGetFilename chooser >>= evaluate
                  else return Nothing
   widgetDestroy chooser
   return returnVal


-- | Asks the user to select a file and sets it to be the new scenario pool on success. Do not call this function from within a thread that owns the Gtk lock!!!
selectScenarioFile :: (ScenarioController ctrl MatrixScenario IO) => MVar (UserInputControl) -> MVar (ScenarioSettings MatrixScenario, ctrl) -> IO Bool
selectScenarioFile uRef sRef = do
   mbPath <- postGUISync showSelectScenarioDialog
   case mbPath of
        Just fp -> do
           (scenSettings, ctrl) <- takeMVar sRef
           uic <- takeMVar uRef
           newScenarios <- readScenario fp
           if (null newScenarios)
             then putMVar uRef uic >> putMVar sRef (scenSettings, ctrl) >> return False
             else do let newScenSettings = setScenarioPool scenSettings newScenarios
                     let newScen = head newScenarios
                     (_, ctrl') <- runStateT (setScenario newScen) ctrl
                     -- there may be a delayed level change -> disable and enable player movement
                     putMVar uRef (uic & lensInputMode %~ (lensMovementMode .~ MovementEnabled) . (lensScenarioChangeMode .~ NoChangeStalled))
                     putMVar sRef (newScenSettings, ctrl')
                     return True
        Nothing -> return False

