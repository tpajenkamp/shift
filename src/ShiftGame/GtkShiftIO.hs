{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module ShiftGame.GtkShiftIO where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans.State.Lazy
import           Data.String
import           Graphics.UI.Gtk hiding (get, set, rectangle)
import           System.Directory
import           System.FilePath
import           System.Glib.UTFString

import ShiftGame.GtkScenarioView
--import ShiftGame.Scenario
import ShiftGame.ScenarioController
import ShiftGame.ShiftIO

{-
Gtk Dialog to choose level file
-}

-- | Asks the user to select a file. Only call this function from a thread that owns the Gtk lock!!!
showSelectScenarioDialog :: GlibFilePath fp => IO (Maybe fp)
showSelectScenarioDialog = do
   chooser <- fileChooserDialogNew (Just (fromString "Select level...")) Nothing FileChooserActionOpen [(stockCancel, ResponseCancel), (stockOpen, ResponseAccept)]
   _ <- catch (do wDir <- getCurrentDirectory
                  let lvlDir = wDir ++ pathSeparator : "levels"
                  lvlDirExist <- doesDirectoryExist lvlDir
                  fileChooserSetCurrentFolder chooser (if lvlDirExist then lvlDir else wDir)) (\(_ :: IOException) -> return False)
   fileChooserSetSelectMultiple chooser False
   fileChooserSetLocalOnly chooser True
   dlgResponse <- dialogRun chooser
   widgetHide chooser
   returnVal <- if dlgResponse == ResponseAccept
                  then fileChooserGetFilename chooser >>= evaluate
                  else return Nothing
   widgetDestroy chooser
   return returnVal


-- | Asks the user to select a file and sets it to be the new scenario pool on success. Do not call this function from within a thread that owns the Gtk lock!!!
selectScenarioFile :: (ParsableScenario sc, ScenarioController ctrl sc IO) => MVar (GameSettings sc, ctrl) -> IO Bool
selectScenarioFile gRef = do
   mbPath <- postGUISync showSelectScenarioDialog
   case mbPath of
        Just fp -> do
           var@(g@(GameSettings scenSettings _ _), ctrl) <- takeMVar gRef
           mbNewScenarios <- readScenario fp
           let newScenarios = maybe [] id mbNewScenarios
           if (null newScenarios)
             then putMVar gRef var >> return False
             else do let newScenSettings = setScenarioPool scenSettings newScenarios
                     let newScen = head newScenarios
                     (_, ctrl') <- runStateT (setScenario newScen) ctrl
                     -- there may be a delayed level change -> disable and enable player movement
                     putMVar gRef (g & (lensUserInputControl . lensMovementMode .~ MovementEnabled)
                                     . (lensScenarioSettings .~ newScenSettings)
                                     . (lensStalledScenarioChange .~ NoChangeStalled)
                                             , ctrl')
                     return True
        Nothing -> return False

