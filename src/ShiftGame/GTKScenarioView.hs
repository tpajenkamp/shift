{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}
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
import           Data.List (partition)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Graphics.Rendering.Cairo (liftIO)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo (surfaceStatus)
import           Graphics.UI.Gtk hiding (get, rectangle)
import           System.Directory (doesFileExist)
import           System.FilePath (pathSeparator)


import ShiftGame.Scenario
import ShiftGame.ScenarioController

data ControlSettings sc = ControlSettings { keysLeft   :: [KeyVal] -- ^ keys (alternatives) to trigger a "left" movement
                                          , keysRight  :: [KeyVal] -- ^ keys (alternatives) to trigger a "right" movement
                                          , keysUp     :: [KeyVal] -- ^ keys (alternatives) to trigger an "up" movement
                                          , keysDown   :: [KeyVal] -- ^ keys (alternatives) to trigger a "down" movement
                                          , keysQuit   :: [KeyVal] -- ^ keys (alternatives) to exit the game
                                          , keysUndo   :: [KeyVal] -- ^ keys (alternatives) to undo a single step
                                          , keysRedo   :: [KeyVal] -- ^ keys (alternatives) to redo a single step
                                          , keysReset  :: [KeyVal] -- ^ keys (alternatives) to restart the level
                                          , keysNext   :: [KeyVal] -- ^ keys (alternatives) to advance to next level
                                          , keysPrev   :: [KeyVal] -- ^ keys (alternatives) to revert to previous level
                                          , scenarioPool    :: [ScenarioState sc] -- ^ currently loaded scenario
                                          , currentScenario :: Int                -- ^ id of current scenario in @scenarioPool@
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



setScenarioPool :: Scenario sc => ControlSettings sc -> [ScenarioState sc] -> ControlSettings sc
setScenarioPool cs s = cs { scenarioPool = s }

setCurrentScenario :: Scenario sc => ControlSettings sc -> Int -> Maybe (ControlSettings sc)
setCurrentScenario cs sId =
  if (sId > 0 && sId <= length (scenarioPool cs))
    then Just $ cs { currentScenario = sId}
    else Nothing

getScenarioFromPool :: Scenario sc => ControlSettings sc -> Int -> ScenarioState sc
getScenarioFromPool cs sId = if (sId >= 0 && sId < length (scenarioPool cs))
                              then scenarioPool cs !! sId
                              else emptyScenarioState

getScenarioFromPoolMaybe :: Scenario sc => ControlSettings sc -> Int -> Maybe (ScenarioState sc)
getScenarioFromPoolMaybe cs sId = if (sId > 0 && sId < length (scenarioPool cs))
                                   then Just $ scenarioPool cs !! sId
                                   else Nothing

isFirstScenarioFromPool :: Scenario sc => ControlSettings sc -> Int -> Bool
isFirstScenarioFromPool cs sId = sId == 0

isFirstScenarioFromPoolCurrent :: Scenario sc => ControlSettings sc-> Bool
isFirstScenarioFromPoolCurrent cs = isFirstScenarioFromPool cs (currentScenario cs)

isLastScenarioFromPool :: Scenario sc => ControlSettings sc -> Int -> Bool
isLastScenarioFromPool cs sId = sId == length (scenarioPool cs) - 1

isLastScenarioFromPoolCurrent :: Scenario sc => ControlSettings sc -> Bool
isLastScenarioFromPoolCurrent cs = isLastScenarioFromPool cs (currentScenario cs)

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
                        in do w <- liftM fromIntegral $ Cairo.imageSurfaceGetWidth sfc  :: Cairo.Render Double
                              h <- liftM fromIntegral $ Cairo.imageSurfaceGetHeight sfc :: Cairo.Render Double
                              Cairo.save >> Cairo.rectangle xc yc (min 48 w) (min 48 h) >> Cairo.clip
                              Cairo.setSourceSurface sfc xc yc >> Cairo.paint >> Cairo.restore
            Nothing -> return ()
        drawPlayer :: (Int, Int) -> (Int, Int) -> Cairo.Render ()
        drawPlayer (lx, ly) c@(x, y) =
            let xc = (fromIntegral (x - lx)) * 48
                yc = (fromIntegral (y - ly)) * 48
            in do img <- case M.lookup (fromMaybe Wall $ getFeature (scenario scs) c) (playerMap imgs) of
                              Just sfc -> return sfc
                              Nothing -> return (playerImg imgs)
                  w <- liftM fromIntegral $ Cairo.imageSurfaceGetWidth img  :: Cairo.Render Double
                  h <- liftM fromIntegral $ Cairo.imageSurfaceGetHeight img :: Cairo.Render Double
                  Cairo.save >> Cairo.rectangle xc yc (min 48 w) (min 48 h) >> Cairo.clip
                  Cairo.setSourceSurface img xc yc >> Cairo.paint >> Cairo.restore  
                              

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
                 Just sfc -> do w <- liftM fromIntegral $ Cairo.imageSurfaceGetWidth sfc  :: Cairo.Render Double
                                h <- liftM fromIntegral $ Cairo.imageSurfaceGetHeight sfc :: Cairo.Render Double
                                Cairo.save >> Cairo.rectangle xcd ycd (min 48 w) (min 48 h) >> Cairo.clip
                                Cairo.setSourceSurface sfc xcd ycd >> Cairo.paint >> Cairo.restore
                 Nothing -> renderEmptyRect xcd ycd 48 48 (1.0, 0.0, 1.0, 1.0)
            return $ Cairo.RectangleInt xc yc 48 48
        drawPlayer :: (Coord, Feature) -> Cairo.Render Cairo.RectangleInt
        drawPlayer item@((x, y), ft) = do
            let xc = (x - lx) * 48
                yc = (y - ly) * 48
                xcd = fromIntegral xc
                ycd = fromIntegral yc
            img <- case M.lookup ft (playerMap imgs) of
                        -- draw combined "Feature+Player" image, if available
                        Just sfc -> return sfc
                        -- draw raw feature and paint player image on top
                        Nothing -> drawFeature item >> return (playerImg imgs)
            w <- liftM fromIntegral $ Cairo.imageSurfaceGetWidth img  :: Cairo.Render Double
            h <- liftM fromIntegral $ Cairo.imageSurfaceGetHeight img :: Cairo.Render Double
            Cairo.save >> Cairo.rectangle xcd ycd (min 48 w) (min 48 h) >> Cairo.clip
            Cairo.setSourceSurface img xcd ycd
            Cairo.paint >> Cairo.restore
            return $ Cairo.RectangleInt xc yc 48 48
            
-- | RGBA color components.
type CairoColor = (Double, Double, Double, Double)

renderEmptyRect :: Double -> Double -> Double -> Double -> CairoColor -> Cairo.Render ()
renderEmptyRect x y w h (r, g, b, a) = do
    Cairo.rectangle x y w h
    Cairo.setSourceRGBA r g b a
    Cairo.fill

createEmptySurface :: Int -> Int -> CairoColor -> IO Cairo.Surface
createEmptySurface w h clr = do
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 w h
    Cairo.renderWith sfc (renderEmptyRect 0 0 (fromIntegral w) (fromIntegral h) clr)
    return sfc

tryLoadPNG :: FilePath -> IO Cairo.Surface
tryLoadPNG path = do
    sfc <- Cairo.imageSurfaceCreateFromPNG path
    status <- Cairo.surfaceStatus sfc
    if (status == Cairo.StatusSuccess)
        then return sfc
        else putStrLn ("invalid PNG file: " ++ path)
          >> createEmptySurface 48 48 (0.0, 1.0, 1.0, 1.0)

drawScenario :: ImagePool -> Cairo.Surface -> ScenarioState MatrixScenario -> IO ()
drawScenario imgs target scs = Cairo.renderWith target (scenarioRender imgs scs)


loadImagePool :: FilePath -> IO ImagePool
loadImagePool parent = do
    (!ftMap, !pMap) <- foldM readFeatureImage (M.empty, M.empty) [minBound..maxBound]
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
                        then tryLoadPNG pathFeature >>= return . (flip . M.insert) ft mFeature
                        else putStrLn ("missing resource file: " ++ pathFeature) >> return mFeature
            existP <- doesFileExist pathWithPlayer
            mPlayer' <- if existP
                         then tryLoadPNG pathWithPlayer >>= return . (flip . M.insert) ft mPlayer
                         else return mPlayer
            return (mFeature', mPlayer')
        getPlayerImage :: IO Cairo.Surface
        getPlayerImage = do
            let playerPath = parent ++ pathSeparator:"Player.png"
            exist <- doesFileExist playerPath
            if exist
              then tryLoadPNG playerPath
              else do putStrLn ("missing resource file: " ++ playerPath)
                      sfc <- Cairo.createImageSurface Cairo.FormatARGB32 48 48
                      Cairo.renderWith sfc (renderEmptyRect 0 0 48 48 (1.0, 0.0, 1.0, 1.0) >>
                                            Cairo.setSourceRGBA 0.0 1.0 0.0 1.0 >>
                                            Cairo.setLineWidth 7.0 >>
                                            Cairo.moveTo 9 9 >>
                                            Cairo.lineTo 38 38 >>
                                            Cairo.moveTo 9 38 >>
                                            Cairo.lineTo 38 9 >>
                                            Cairo.stroke
                                            )
                      return sfc


copyScenarioToSurface :: IORef Cairo.Surface -> Cairo.Render ()
copyScenarioToSurface mapSurfaceRef = do
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
    _ <- drawin `on` draw $ copyScenarioToSurface scenRef
    return $ CanvasUpdateListener imgs drawin scenRef (lx, ly)


data StatusBarListener sc = StatusBarListener Statusbar ContextId

instance Scenario sc => UpdateListener (StatusBarListener sc) IO sc where
  notifyUpdate :: (StatusBarListener sc) -> ScenarioUpdate -> ReaderT (ScenarioState sc) IO (StatusBarListener sc)
  notifyUpdate l@(StatusBarListener bar cId) u = do
      let (steps, steps') = updatedSteps u
      _ <- lift $ statusbarPush bar cId (show steps ++ " / " ++ show steps')
      return l
  notifyNew :: (StatusBarListener sc) -> ReaderT (ScenarioState sc) IO (StatusBarListener sc)
  notifyNew l@(StatusBarListener bar cId) = do
      scs <- ask
      let (steps, steps') = spentSteps scs
      _ <- lift $ statusbarPush bar cId (show steps ++ " / " ++ show steps')
      return l
  notifyWin :: (StatusBarListener sc) -> ReaderT (ScenarioState sc) IO (StatusBarListener sc)
  notifyWin l@(StatusBarListener bar cId) = do
      scs <- ask
      let (steps, steps') = spentSteps scs
      _ <- lift $ statusbarPush bar cId ("Victory! " ++ show steps ++ " / " ++ show steps')
      return l

createStatusBarLink :: Scenario sc => Statusbar -> IO (StatusBarListener sc)
createStatusBarLink bar = do
    contextId <- statusbarGetContextId bar "Steps"
    return $ StatusBarListener bar contextId
    

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
                         then do let currentScen = getScenarioFromPool ctrlSettings (currentScenario ctrlSettings)
                                 (_, newState) <- lift $ runStateT (setScenario currentScen) ctrlState
                                 lift $ putStrLn "level reset"
                                 (lift . writeIORef ref) (ctrlSettings, newState)
                                 return True
                         else if (keyV `elem` keysNext ctrlSettings)
                         then do unless (isLastScenarioFromPoolCurrent ctrlSettings) $ do
                                   let nextScen = getScenarioFromPool ctrlSettings (currentScenario ctrlSettings + 1)
                                   (_, newState) <- lift $ runStateT (setScenario nextScen) ctrlState
                                   lift $ putStrLn "next level"
                                   (lift . writeIORef ref) (ctrlSettings { currentScenario = currentScenario ctrlSettings + 1 }, newState)
                                 return True
                         else if (keyV `elem` keysPrev ctrlSettings)
                         then do unless (isFirstScenarioFromPoolCurrent ctrlSettings) $ do
                                   let prevScen = getScenarioFromPool ctrlSettings (currentScenario ctrlSettings - 1)
                                   (_, newState) <- lift $ runStateT (setScenario prevScen) ctrlState
                                   lift $ putStrLn "previous level"
                                   (lift . writeIORef ref) (ctrlSettings { currentScenario = currentScenario ctrlSettings - 1}, newState)
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


                         
                         
