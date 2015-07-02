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
import           Data.List (find, partition)
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



{-
TextView based view
-}

data TextViewUpdateListener = TextViewUpdateListener TextBuffer

instance UpdateListener TextViewUpdateListener IO MatrixScenario where
  notifyUpdate :: TextViewUpdateListener -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO TextViewUpdateListener
  notifyUpdate l@(TextViewUpdateListener tBuffer) _ = do
      scState <- ask -- todo: player position
      let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
      (lift . textBufferSetByteString tBuffer) levelStrWithPlayer
      return l
  notifyNew :: TextViewUpdateListener -> ReaderT (ScenarioState MatrixScenario) IO TextViewUpdateListener
  notifyNew l@(TextViewUpdateListener tBuffer) = do
      scState <- ask -- todo: player position
      let levelStrWithPlayer = showScenarioWithPlayer (scenario scState) (playerCoord scState)
      (lift . textBufferSetByteString tBuffer) levelStrWithPlayer
      return l
  notifyWin :: TextViewUpdateListener -> ReaderT (ScenarioState MatrixScenario) IO TextViewUpdateListener
  notifyWin l@(TextViewUpdateListener tBuffer) = do lift $ putStrLn "you win!" >> return l



setInitialScenario :: Scenario sc => ControlSettings sc -> ScenarioState sc -> ControlSettings sc
setInitialScenario cs s = cs { initialScenario = s }

createTextViewLink :: TextBuffer -> TextViewUpdateListener
createTextViewLink tBuffer = TextViewUpdateListener tBuffer




{-
Graphics View
-}

data ImagePool = ImagePool { featureMap :: M.Map Feature Cairo.Surface -- ^ map containing images for raw features
                           , playerMap  :: M.Map Feature Cairo.Surface -- ^ map containing images for player onto feature, if special
                           , playerImg  :: Cairo.Surface               -- ^ player image to draw onto feature if not contained in playerMap
                           }

data CanvasUpdateListener = CanvasUpdateListener { bufferedImages :: ImagePool            -- ^ Available images to draw onto canvas
                                                 , drawCanvas     :: DrawingArea          -- ^ Connected @DrawingArea@ serving as canvas
                                                 , surfaceRef     :: IORef Cairo.Surface  -- ^ Reference to Cairo image of currently drawed scenario
                                                 , lowScenarioBnd :: (Int, Int)           -- ^ Lower (x, y) bounds of current scenario
                                                 }

instance UpdateListener CanvasUpdateListener IO MatrixScenario where
  notifyUpdate :: CanvasUpdateListener -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO CanvasUpdateListener
  notifyUpdate l@(CanvasUpdateListener imgs widget sfcRef lowBnd) u = do
      sfc <- lift $ readIORef sfcRef
      invalRegion <- lift $ Cairo.renderWith sfc (scenarioUpdateRender imgs u lowBnd)
      lift $ widgetQueueDrawRegion widget invalRegion
      return l
  notifyNew :: CanvasUpdateListener -> ReaderT (ScenarioState MatrixScenario) IO CanvasUpdateListener
  notifyNew l@(CanvasUpdateListener imgs widget sfcRef _) = do
      sfc <- lift $ readIORef sfcRef
      scs <- ask
      -- create new surface if dimension changed
      w <- Cairo.imageSurfaceGetWidth sfc
      h <- Cairo.imageSurfaceGetHeight sfc
      let ((lx,ly), (hx, hy)) = getMatrixScenarioBounds (scenario scs)
          xSpan = (hx-lx + 1) * 48
          ySpan = (hy-ly + 1) * 48
      when (w /= xSpan || h /= ySpan) $ lift $ do
          newSfc <- Cairo.createImageSurface Cairo.FormatARGB32 xSpan ySpan
          writeIORef sfcRef newSfc
          widgetSetSizeRequest widget xSpan ySpan
      -- redraw scenario surface
      lift $ drawScenario imgs sfc scs
      lift $ widgetQueueDraw widget
      return l { lowScenarioBnd = (lx,ly) }
  notifyWin :: CanvasUpdateListener -> ReaderT (ScenarioState MatrixScenario) IO CanvasUpdateListener
  notifyWin l = do lift $ putStrLn "fancy: you win!" >> return l

scenarioRender :: ImagePool -> ScenarioState MatrixScenario -> Cairo.Render ()
scenarioRender imgs scs = do
    -- paint magent rectangle for invalid textures
    (cx1, cy1, cx2, cy2) <- Cairo.clipExtents
    Cairo.rectangle cx1 cy1 cx2 cy2
    Cairo.setSourceRGB 1.0 0.0 1.0
    Cairo.fill
    -- paint scenario map
    let (l@(lx,ly), (hx, hy)) = getMatrixScenarioBounds (scenario scs)
    sequence_ $ map (drawFeature l) [(x, y) | x <- [lx..hx], y <- [ly..hy]]
    -- paint player
    drawPlayer l (playerCoord scs)
  where drawFeature :: (Int, Int) -> (Int, Int) -> Cairo.Render ()
        drawFeature (lx, ly) c@(x, y) = case M.lookup (fromMaybe Wall $ getFeature (scenario scs) c) (featureMap imgs) of
            Just sfc -> let xc = (fromIntegral (x - lx)) * 48
                            yc = (fromIntegral (y - ly)) * 48
                        in Cairo.setSourceSurface sfc xc yc >> Cairo.paint
            Nothing -> return ()
        drawPlayer :: (Int, Int) -> (Int, Int) -> Cairo.Render ()
        drawPlayer (lx, ly) c@(x, y) =
            let xc = (fromIntegral (x - lx)) * 48
                yc = (fromIntegral (y - ly)) * 48
            in case M.lookup (fromMaybe Wall $ getFeature (scenario scs) c) (playerMap imgs) of
                    Just sfc -> Cairo.setSourceSurface sfc xc yc >> Cairo.paint
                    Nothing -> Cairo.setSourceSurface (playerImg imgs) xc yc >> Cairo.paint

scenarioUpdateRender :: ImagePool                    -- ^ single sprites
                     -> ScenarioUpdate               -- ^ new scenario state
                     -> (Int, Int)                   -- ^ lower (x, y) scenario bounds
                     -> Cairo.Render (Cairo.Region)
scenarioUpdateRender imgs u (lx, ly) = do
    -- overpaint given coordinates
    let pc = newPlayerCoord u
        -- find current player position in update list
        (pCoord, pNotCoord) = partition (\(c, _) -> c == pc) (changedFeatures u)
    inval  <- sequence $ map drawFeature pNotCoord
    inval' <- sequence $ map drawPlayer pCoord
    Cairo.regionCreateRectangles (inval ++ inval')
  where drawFeature :: (Coord, Feature) -> Cairo.Render Cairo.RectangleInt
        drawFeature ((x, y), ft) = do
            let xc = (x - lx) * 48
                yc = (y - ly) * 48
                xcd = fromIntegral xc :: Double
                ycd = fromIntegral yc :: Double
            case M.lookup ft (featureMap imgs) of
                 Just sfc -> Cairo.setSourceSurface sfc xcd ycd >> Cairo.paint
                 Nothing -> do Cairo.rectangle xcd ycd 48 48
                               Cairo.setSourceRGB 1.0 0.0 1.0
                               Cairo.fill
            return $ Cairo.RectangleInt xc yc 48 48
        drawPlayer :: (Coord, Feature) -> Cairo.Render Cairo.RectangleInt
        drawPlayer item@((x, y), ft) = do
            let xc = (x - lx) * 48
                yc = (y - ly) * 48
                xcd = fromIntegral xc
                ycd = fromIntegral yc
            case M.lookup ft (playerMap imgs) of
                 -- draw combined "Feature+Player" image, if available
                 Just sfc -> Cairo.setSourceSurface sfc xcd ycd >> Cairo.paint
                 -- draw raw feature and paint player image on top
                 Nothing -> do drawFeature item
                               Cairo.setSourceSurface (playerImg imgs) xcd ycd
                               Cairo.paint
            return $ Cairo.RectangleInt xc yc 48 48

drawScenario :: ImagePool -> Cairo.Surface -> ScenarioState MatrixScenario -> IO ()
drawScenario imgs target scs = Cairo.renderWith target (scenarioRender imgs scs)


loadImagePool :: FilePath -> IO ImagePool
loadImagePool parent = do
    (ftMap, pMap) <- foldM readFeatureImage (M.empty, M.empty) [minBound..maxBound]
    pImg <- getPlayerImage
    return $ ImagePool ftMap pMap pImg
  where -- | Tries to find Feature image (<parent_path>/<feature>.png) and
        --   Feature image with player (<parent_path>/<feature>_Player.png), adds them to map if possible
        readFeatureImage :: (M.Map Feature Cairo.Surface, M.Map Feature Cairo.Surface)    -- ^ (Feature map, Feature+Player map)
                         -> Feature                                                       -- ^ Feature to search image for
                         -> IO (M.Map Feature Cairo.Surface, M.Map Feature Cairo.Surface)
        readFeatureImage (mFeature, mPlayer) ft = do
            let pathFeature = (parent ++ pathSeparator:(show ft) ++ ".png")
                pathWithPlayer =  (parent ++ pathSeparator:(show ft) ++ "_Player.png")
            exist <- doesFileExist pathFeature
            mFeature' <- if exist
                        then Cairo.imageSurfaceCreateFromPNG pathFeature >>= return . (flip . M.insert) ft mFeature
                        else return mFeature
            existP <- doesFileExist pathWithPlayer
            mPlayer' <- if existP
                         then Cairo.imageSurfaceCreateFromPNG pathWithPlayer >>= return . (flip . M.insert) ft mPlayer
                         else return mPlayer
            return (mFeature', mPlayer')
        getPlayerImage :: IO Cairo.Surface
        getPlayerImage = do
            let playerPath = parent ++ pathSeparator:"Player.png"
            exist <- doesFileExist playerPath
            if exist
              then Cairo.imageSurfaceCreateFromPNG playerPath
              else do sfc <- Cairo.createImageSurface Cairo.FormatARGB32 48 48
                      Cairo.renderWith sfc (Cairo.rectangle 0 0 48 48 >>
                                            Cairo.setSourceRGBA 1.0 0.0 1.0 0.0 >>
                                            Cairo.fill >>
                                            Cairo.setSourceRGBA 0.0 1.0 0.0 1.0 >>
                                            Cairo.setLineWidth 7.0 >>
                                            Cairo.moveTo 9 9 >>
                                            Cairo.lineTo 38 38 >>
                                            Cairo.moveTo 9 38 >>
                                            Cairo.lineTo 38 9 >>
                                            Cairo.stroke
                                            )
                      return sfc


copyScenarioToSurface :: ImagePool -> IORef Cairo.Surface -> Cairo.Render ()
copyScenarioToSurface imgs mapSurfaceRef = do
    mapSurface <- liftIO $ readIORef mapSurfaceRef
    Cairo.setSourceSurface mapSurface 0 0
    Cairo.paint


createCanvasViewLink :: ImagePool -> DrawingArea -> ScenarioState MatrixScenario -> IO CanvasUpdateListener
createCanvasViewLink imgs drawin scs = do
    let sc = scenario scs
        ((lx,ly), (hx, hy)) = getMatrixScenarioBounds sc
        xSpan = (hx-lx + 1) * 48
        ySpan = (hy-ly + 1) * 48
    scenSurface <- Cairo.createImageSurface Cairo.FormatARGB32 xSpan ySpan
    scenRef <- newIORef scenSurface
    widgetSetSizeRequest drawin xSpan ySpan
    drawin `on` draw $ copyScenarioToSurface imgs scenRef
    return $ CanvasUpdateListener imgs drawin scenRef (lx, ly)


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


                         
                         
