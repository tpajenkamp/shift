{-# LANGUAGE FlexibleContexts #-}
module ShiftGame.GtkShiftIO where

import           Control.Concurrent
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

showSelectScenarioDialog :: GlibFilePath fp => IO (Maybe fp)
showSelectScenarioDialog = do
   chooser <- fileChooserDialogNew (Just "Select level...") Nothing FileChooserActionOpen [("_Cancel", ResponseCancel), ("_Open", ResponseAccept)]
   fileChooserSetSelectMultiple chooser False
   response <- dialogRun chooser
   widgetHide chooser
   print response
   returnVal <- if response == ResponseAccept
                  then fileChooserGetFilename chooser
                  else return Nothing
   widgetDestroy chooser
   return returnVal

selectScenarioFile :: (Scenario sc, ScenarioController ctrl sc IO) => MVar (UserInputControl) -> MVar (ScenarioSettings sc, ctrl) -> IO Bool
selectScenarioFile uRef sRef = do
   mbPath <- showSelectScenarioDialog
   case mbPath of
        Just fp -> return True -- TODO: select scenario
        Nothing -> return False

-- TODO: add key (o) to open FileChooserDialog and set level

