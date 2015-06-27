{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Array as A
import qualified Data.ByteString.Char8 as B
import           Data.IORef
import           Data.List (find)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Graphics.Rendering.Cairo (liftIO)
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk hiding (get, rectangle)
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath (takeFileName, dropExtensions, pathSeparator)
import           Text.Read (readMaybe)

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


setInitialScenario :: Scenario sc => ControlSettings sc -> ScenarioState sc -> ControlSettings sc
setInitialScenario cs s = cs { initialScenario = s }


{-
TextView based view
-}

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


{-
Graphics View
-}

data ImagePool = ImagePool { featureMap :: M.Map Feature Cairo.Surface -- ^ map containing images for raw features
                           , playerMap  :: M.Map Feature Cairo.Surface -- ^ map containing images for player onto feature, if special
                           , playerImg  :: Cairo.Surface               -- ^ player image to draw onto feature if not contained in playerMap
                           }

loadImagePool :: FilePath -> IO ImagePool
loadImagePool parent = do
    content <- getDirectoryContents parent
    let dict = M.empty :: M.Map Feature Cairo.Surface
        contentWithPath = map (\f -> parent ++ pathSeparator:f) content
    ftMap <- join $ fmap (foldM addIfFeature dict) (filterM doesFileExist contentWithPath)
    pImg <- getPlayerImage content
    return $ ImagePool ftMap M.empty pImg
  where addIfFeature :: M.Map Feature Cairo.Surface -> FilePath -> IO (M.Map Feature Cairo.Surface)
        addIfFeature m path = do
            let fileName = (takeFileName . dropExtensions) path
            case readMaybe fileName :: Maybe Feature of
                 Nothing -> return m
                 Just f -> do
                     s <- Cairo.imageSurfaceCreateFromPNG path
                     return $ M.insert f s m
        getPlayerImage :: [FilePath] -> IO Cairo.Surface
        getPlayerImage content =
            case find ((==) "Player" . dropExtensions) content of
                 Just p -> Cairo.imageSurfaceCreateFromPNG (parent ++ pathSeparator:p)
                 Nothing -> do
                     sfc <- Cairo.createImageSurface Cairo.FormatARGB32 48 48
                     Cairo.renderWith sfc (Cairo.rectangle 0 0 48 48 >>
                                           Cairo.setSourceRGBA 1.0 0.0 1.0 0.0 >>
                                           Cairo.fill >>
                                           Cairo.setSourceRGBA 0.0 1.0 0.0 1.0 >>
                                           Cairo.setLineWidth 5.0 >>
                                           Cairo.moveTo 9 9 >>
                                           Cairo.lineTo 38 38 >>
                                           Cairo.paint
                                           )
                     return sfc


fullScenarioRenderer :: ImagePool -> IORef Cairo.Surface -> Cairo.Render ()
fullScenarioRenderer imgs mapSurfaceRef = do
    mapSurface <- liftIO $ readIORef mapSurfaceRef
    Cairo.setSourceSurface mapSurface 0 0
    Cairo.paint

-- todo: UpdateListener as class

scenarioRender :: ImagePool -> ScenarioState MatrixScenario -> Cairo.Render ()
scenarioRender imgs scs = do
    -- paint magent rectangle for invalid textures
    (cx1, cy1, cx2, cy2) <- Cairo.clipExtents
    Cairo.rectangle cx1 cy1 cx2 cy2
    Cairo.setSourceRGB 1.0 0.0 1.0
    Cairo.fill
    -- paint scenario map
    let ((lx,ly), (hx, hy)) = getMatrixScenarioBounds (scenario scs)
    sequence_ $ map drawFeature [(x, y) | x <- [lx..hx], y <- [ly..hy]] -- todo: ix lx or ly /= 0
    -- paint player
    drawPlayer (playerCoord scs)
  where drawFeature :: (Int, Int) -> Cairo.Render ()
        drawFeature c@(x, y) = case M.lookup (fromMaybe Wall $ getFeature (scenario scs) c) (featureMap imgs) of
            Just sfc -> let xc = (fromIntegral x) * 48
                            yc = (fromIntegral y) * 48
                        in Cairo.setSourceSurface sfc xc yc >> Cairo.paint
            Nothing -> return ()
        drawPlayer :: (Int, Int) -> Cairo.Render ()
        drawPlayer c@(x, y) =
            let xc = (fromIntegral x) * 48
                yc = (fromIntegral y) * 48
            in case M.lookup (fromMaybe Wall $ getFeature (scenario scs) c) (playerMap imgs) of
                    Just sfc -> Cairo.setSourceSurface sfc xc yc >> Cairo.paint
                    Nothing -> Cairo.setSourceSurface (playerImg imgs) xc yc >> Cairo.paint

drawScenario :: ImagePool -> Cairo.Surface -> ScenarioState MatrixScenario -> IO ()
drawScenario imgs target scs = Cairo.renderWith target (scenarioRender imgs scs)

createCanvasViewLink :: ImagePool -> DrawingArea -> ScenarioState MatrixScenario -> IO (UpdateListener IO MatrixScenario)
createCanvasViewLink imgs drawin scs = do
    let ((lx,ly), (hx, hy)) = getMatrixScenarioBounds (scenario scs)
    scenSurface <- Cairo.createImageSurface Cairo.FormatARGB32 ((hx-lx + 1) * 48) ((hy-ly + 1) * 48)
    scenRef <- newIORef scenSurface
    drawin `on` draw $ fullScenarioRenderer imgs scenRef
    return $ UpdateListener (canvasViewUpdateFunction imgs drawin scenRef) (canvasViewCreateFunction imgs drawin scenRef) (canvasViewWinFunction imgs drawin scenRef)

canvasViewUpdateFunction :: ImagePool -> DrawingArea -> IORef Cairo.Surface -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO ()
canvasViewUpdateFunction imgs widget scenRef _ = do
    scen <- lift $ readIORef scenRef
    scs <- ask
    lift $ drawScenario imgs scen scs
    lift $ widgetQueueDraw widget

canvasViewCreateFunction :: ImagePool -> DrawingArea -> IORef Cairo.Surface -> ReaderT (ScenarioState MatrixScenario) IO ()
canvasViewCreateFunction imgs widget scenRef = do
    scen <- lift $ readIORef scenRef
    scs <- ask
    lift $ drawScenario imgs scen scs
    lift $ widgetQueueDraw widget

canvasViewWinFunction :: ImagePool -> DrawingArea -> IORef Cairo.Surface-> ReaderT (ScenarioState MatrixScenario) IO ()
canvasViewWinFunction imgs widget scenRef = do lift $ putStrLn "you win!"

{-
Keyboard Listener
-}

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


                         
                         
