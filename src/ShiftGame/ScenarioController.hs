{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances, InstanceSigs, ExistentialQuantification, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  ShiftGame.ScenarioParser
-- Copyright   :  (c) 2015, Thomas Pajenkamp
-- License     :  BSD3
--
-- Maintainer  :  tpajenka
-- Stability   :
-- Portability :
--
-- | Controller for communication between view and game model
--
-----------------------------------------------------------------------------

module ShiftGame.ScenarioController where

import Control.Lens
--import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import LensNaming
import ShiftGame.Scenario

{-
Scenario Pool and its basic operations
-}

-- | Id of a 'Scenario' within a 'ScenarioSettings' object.
type ScenarioId = Int

-- | Storage for a bunch of @Scenario@s and the id of the currently active scenario.
data ScenarioSettings sc = ScenarioSettings { scenarioPool    :: [ScenarioState sc] -- ^ loaded scenarios
                                            , currentScenario :: ScenarioId         -- ^ id of current scenario in @scenarioPool@, starting with @0@
                                            } deriving (Eq, Show, Read)
$(makeLensPrefixLenses ''ScenarioSettings)

-- | Sets the stored scenarios. All previous scenarios are discarded and the current scenario is set to the first.
setScenarioPool :: Scenario sc => ScenarioSettings sc -> [ScenarioState sc] -> (ScenarioSettings sc)
setScenarioPool cs s = cs & (lensScenarioPool .~ s) & (lensCurrentScenario .~ 0)

-- | Sets the currently selected scenario. Returns 'Nothing' if the given @ScenarioId@ is outside range.
setCurrentScenario :: Scenario sc => ScenarioSettings sc    -- ^ Current @ScenarioSettings@ object
                                  -> ScenarioId             -- ^ id of the new current scenario
                                  -> Maybe (ScenarioSettings sc, ScenarioState sc)    -- ^ updated input @ScenarioSettings@ and the scenario of the given id
setCurrentScenario cs sId =
  if (sId > 0 && sId <= length (scenarioPool cs))
    then Just (set lensCurrentScenario sId cs, scenarioPool cs !! sId)
    else Nothing

-- | Returns the scenario of the given id. Returns a default empty scenario if the id is outside range.
--
--   Use 'getScenarioFromPoolMaybe' to get feedback on the range.
getScenarioFromPool :: Scenario sc => ScenarioSettings sc    -- ^ current @ScenarioSettings@
                                   -> ScenarioId             -- ^ requested scenario id
                                   -> ScenarioState sc       -- ^ selected scenario
getScenarioFromPool cs sId = if (sId >= 0 && sId < length (scenarioPool cs))
                              then scenarioPool cs !! sId
                              else emptyScenarioState

-- | Returns the scenario of the given id. Returns 'Nothing' if the id is outside range.
--
--   Use 'getScenarioFromPool' to get a default scenario in case of failure.
getScenarioFromPoolMaybe :: Scenario sc => ScenarioSettings sc    -- ^ current @ScenarioSettings@
                                        -> ScenarioId             -- ^ requested scenario id
                                        -> Maybe (ScenarioState sc)    --  ^ selected scenario
getScenarioFromPoolMaybe cs sId = if (sId > 0 && sId < length (scenarioPool cs))
                                   then Just $ scenarioPool cs !! sId
                                   else Nothing

-- | Sets the next scenario in the scenario pool to be the current scenario. Returns 'Nothing' if there is no next scenario.
increaseScenarioId :: Scenario sc => ScenarioSettings sc    -- ^ current @ScenarioSettings@
                                  -> Maybe (ScenarioSettings sc, ScenarioState sc, ScenarioId)    -- ^ updated @ScenarioSettings@, next scenario and its id
increaseScenarioId cs = let currentScenarioId = currentScenario cs
   in if isLastScenarioFromPool cs currentScenarioId
        then Nothing
        else Just (cs & lensCurrentScenario .~ currentScenarioId + 1, scenarioPool cs !! (currentScenarioId + 1), currentScenarioId + 1) 

-- | Sets the previous scenario in the scenario pool to be the current scenario. Returns 'Nothing' if there is no previous scenario.
decreaseScenarioId :: Scenario sc => ScenarioSettings sc    -- ^ current @ScenarioSettings@
                                  -> Maybe (ScenarioSettings sc, ScenarioState sc, ScenarioId)    -- ^ updated @ScenarioSettings@, previous scenario and its id
decreaseScenarioId cs = let currentScenarioId = currentScenario cs
   in if isFirstScenarioFromPool cs currentScenarioId
        then Nothing
        else Just (cs & lensCurrentScenario .~ currentScenarioId - 1, scenarioPool cs !! (currentScenarioId - 1), currentScenarioId - 1) 

-- | Returns 'True' if and only if there is no scenario after the specified scenario in the scenario pool.
isFirstScenarioFromPool :: Scenario sc => ScenarioSettings sc -> ScenarioId -> Bool
isFirstScenarioFromPool _ sId = sId <= 0

-- | Returns 'True' if and only if there is no scenario after the current scenario in the scenario pool.
isFirstScenarioFromPoolCurrent :: Scenario sc => ScenarioSettings sc-> Bool
isFirstScenarioFromPoolCurrent cs = isFirstScenarioFromPool cs (currentScenario cs)

-- | Returns 'True' if and only if there is no scenario before the specified scenario in the scenario pool.
isLastScenarioFromPool :: Scenario sc => ScenarioSettings sc -> ScenarioId -> Bool
isLastScenarioFromPool cs sId = sId >= length (scenarioPool cs) - 1

-- | Returns 'True' if and only if there is no scenario before the current scenario in the scenario pool.
isLastScenarioFromPoolCurrent :: Scenario sc => ScenarioSettings sc -> Bool
isLastScenarioFromPoolCurrent cs = isLastScenarioFromPool cs (currentScenario cs)


{-
Input-Model communication and model listeners
-}

-- | A listener that can be informed about @ScenarioUpdate@s.
class UpdateListener u m sc | u -> m sc where
  -- | Notification about an update in the scenario (usually a player movement).
  --
  --   The expected return value is the possibly changed listener after processing the event.
  notifyUpdate :: u -> ScenarioUpdate -> ReaderT (ScenarioState sc) m u
  notifyUpdate u _ = notifyNew u
  -- | Notification about a new scenario or a scenario reset. All previous data is considered invalid.
  --
  --   The expected return value is the possibly changed listener after processing the event.
  notifyNew    :: u -> ReaderT (ScenarioState sc) m u
  -- | Notification about the end of a scenario because the player has won. Is sent after the corresponding 'notifyUpdate'.
  --
  --   The expected return value is the possibly changed listener after processing the event.
  notifyWin    :: u -> ReaderT (ScenarioState sc) m u

-- | A wrapper for any 'UpdateListener' data.
data UpdateListenerType m sc = forall a. UpdateListener a m sc => UpdateListenerType a

instance Monad m => UpdateListener (UpdateListenerType m sc) m sc where
  notifyUpdate (UpdateListenerType l) u = notifyUpdate l u >>= (return . UpdateListenerType)
  notifyNew (UpdateListenerType l) = notifyNew l >>= (return . UpdateListenerType)
  notifyWin (UpdateListenerType l) = notifyWin l >>= (return . UpdateListenerType)

-- | Cotroller for propagating and executing player commands and linking them to the game logic.
--   
--  Class parameters
--  
--   * @ctrl@ the controller itself
--
--   * @cs@ internal controller state
--
--   * @sc@ 'Scenario' instance
--
--   * @m@ 'Monad' of related 'UpdateListener'
--
class (Scenario sc, Monad m) => ScenarioController ctrl sc (m :: * -> *) | ctrl -> sc m where
    -- | Initial controller state with the given scenario and without any listeners.
    initControllerState :: ScenarioState sc -> ctrl
    -- | Returns current @ScenarioState@.
    getControllerScenarioState :: ctrl -> ScenarioState sc
    -- | Tries to move the player into the specified direction.
    --   Notifies all registered listeners on success and returns 'Nothing'.
    --   If the move is not possible returns the reason.
    runPlayerMove :: PlayerMovement -> StateT ctrl m (Maybe DenyReason)
    -- | Adds a listener to be notified about a changed scenario.
    --   The added listener is immediately informed about the current scenario state.
    addListener :: (Monad m, UpdateListener u m sc) => u -> StateT ctrl m ()
    -- | Aborts the current game and sets a new scenario.
    setScenario :: ScenarioState sc -> StateT ctrl m ()
    -- | Reverts a single step, returns @Nothing@ on success. Optional.
    undoAction :: StateT ctrl m (Maybe DenyReason)
    undoAction = return (Just ActionUnsupported)
    -- | Reverts a single step, returns @Nothing@ on success. Optional.
    redoAction :: StateT ctrl m (Maybe DenyReason)
    redoAction = return (Just ActionUnsupported)

-- | Sets the scenario in the internal controller state.
controllerSetScenario :: (Scenario sc, Functor m, ScenarioController ctrl sc m) => ctrl                -- ^ current controller
                                                                                -> ScenarioState sc    -- ^ new scenario
                                                                                -> m ctrl              -- ^ updated controller
controllerSetScenario ctrl s = (fmap snd) $ runStateT (setScenario s) ctrl

-- | Adds a listener to the controller. The new listener is informed about the current state.
controllerAddListener :: (Scenario sc, Functor m, ScenarioController ctrl sc m, UpdateListener u m sc) => ctrl      -- ^ current controller
                                                                                                       -> u         -- ^ new listener
                                                                                                       -> m ctrl    -- ^ updated controller
controllerAddListener ctrl l = (fmap snd) $ runStateT (addListener l) ctrl

-- | Creates a new controller with the given scenario.
initController :: (Functor m, ScenarioController ctrl sc m, UpdateListener u m sc) => ScenarioState sc -> u -> m ctrl
initController sc lst = fmap snd $ runStateT (addListener lst) (initControllerState sc)

-- | The state of a controller handling communication between model and view
data ControllerState m sc = ControllerState { scenarioState :: ScenarioState sc           -- ^ the game state
                                            , listeners     :: [UpdateListenerType m sc]  -- ^ all known listeners
                                            }
$(makeLensPrefixLenses ''ControllerState)


instance Show sc => Show (ControllerState m sc) where
    show (ControllerState scen lst) = "ControllerState {scenarioState = " ++ show scen ++ ", listeners = (" ++ (show . length) lst ++ "listeners )}"


instance (Scenario sc, Monad m) => ScenarioController (ControllerState m sc) sc m where
    getControllerScenarioState = scenarioState
    initControllerState = flip ControllerState []
    runPlayerMove move = do cs <- get
                            let s = scenarioState cs
                            case askPlayerMove s move of
                                 (Left reason) -> (return . Just) reason    -- propagate reason for invalid move
                                 (Right update) -> do                       -- update internal ScenarioState
                                     let (Right s') = updateScenario s update
                                     put $ set lensScenarioState s' cs
                                     cs' <- get
                                     -- inform all listeners about change
                                     lst <- lift $ sequence $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate update) (listeners cs')
                                     -- test winning condition
                                     lst' <- if (isWinningState s')
                                         then lift $ sequence $ map (flip runReaderT (scenarioState cs') . notifyWin) lst
                                         else return lst
                                     modify (\cs -> set lensListeners lst' cs)
                                     return Nothing
    addListener l = do cs <- get
                       l' <- lift $ runReaderT (notifyNew l) (scenarioState cs)    -- inform new listener
                       modify (\cs -> over lensListeners (UpdateListenerType l' :) cs)
                       return ()
    setScenario sc = do modify (\cs -> set lensScenarioState sc cs)
                        cs <- get
                        lst <- lift $ sequence $ map (flip runReaderT (scenarioState cs) . notifyNew) (listeners cs)
                        modify (\cs -> set lensListeners lst cs)
                        return ()
    undoAction = do cs <- get
                    case undo (scenarioState cs) of
                         Left r -> (return . Just) r
                         Right (u, scs) -> do put (set lensScenarioState scs cs)
                                              cs' <- get
                                              lst <- lift $ sequence $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate u) (listeners cs)
                                              modify (\cs -> set lensListeners lst cs)
                                              return Nothing
    redoAction = do cs <- get
                    case redo (scenarioState cs) of
                         Left r -> (return . Just) r
                         Right (u, scs) -> do put (set lensScenarioState scs cs)
                                              cs' <- get
                                              lst <- lift $ sequence $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate u) (listeners cs)
                                              -- test winning condition
                                              lst' <- if (isWinningState scs)
                                                then lift $ sequence $ map (flip runReaderT (scenarioState cs') . notifyWin) lst
                                                else return lst
                                              -- update ControllerState
                                              modify (\cs -> set lensListeners lst' cs)
                                              return Nothing

