{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances, InstanceSigs, ExistentialQuantification #-}
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

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import ShiftGame.Scenario

-- | A listener that can be informed about @ScenarioUpdate@s.
class UpdateListener u m sc | u -> m sc where
  notifyUpdate :: u -> ScenarioUpdate -> ReaderT (ScenarioState sc) m ()
  notifyUpdate u _ = notifyNew u
  notifyNew    :: u -> ReaderT (ScenarioState sc) m ()
  notifyWin    :: u -> ReaderT (ScenarioState sc) m ()

-- | A wrapper for any 'UpdateListener' data.
data UpdateListenerType m sc = forall a. UpdateListener a m sc => UpdateListenerType a

instance UpdateListener (UpdateListenerType m sc) m sc where
  notifyUpdate (UpdateListenerType l) u = notifyUpdate l u
  notifyNew (UpdateListenerType l) = notifyNew l
  notifyWin (UpdateListenerType l) = notifyWin l

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

controllerSetScenario :: (Scenario sc, Functor m, ScenarioController ctrl sc m) => ctrl -> ScenarioState sc -> m ctrl
controllerSetScenario ctrl s = (fmap snd) $ runStateT (setScenario s) ctrl

controllerAddListener :: (Scenario sc, Functor m, ScenarioController ctrl sc m, UpdateListener u m sc) => ctrl -> u -> m ctrl
controllerAddListener ctrl l = (fmap snd) $ runStateT (addListener l) ctrl

initController :: (Functor m, ScenarioController ctrl sc m, UpdateListener u m sc) => ScenarioState sc -> u -> m ctrl
initController sc lst = fmap snd $ runStateT (addListener lst) (initControllerState sc)

-- | The state of a controller handling communication between model and view
data ControllerState m sc = ControllerState { scenarioState :: ScenarioState sc       -- ^ the game state
                                            , listeners     :: [UpdateListenerType m sc]  -- ^ all known listeners
                                            }


instance Show sc => Show (ControllerState m sc) where
    show (ControllerState scen lst) = "ControllerState {scenarioState = " ++ show scen ++ ", listeners = (" ++ (show . length) lst ++ "listeners )}"


-- todo: not working, m and sc need to be derived from ctrl
-- otherwise it is required to work with EVERY monad and scenario type
instance (Scenario sc, Monad m) => ScenarioController (ControllerState m sc) sc m where
    initControllerState = flip ControllerState []
    runPlayerMove move = do cs <- get
                            let s = scenarioState cs
                            case askPlayerMove s move of
                                 (Left reason) -> (return . Just) reason    -- propagate reason for invalid move
                                 (Right update) -> do                  -- update internal ScenarioState
                                     let (Right s') = updateScenario s update
                                     put $ cs { scenarioState = s' }
                                     cs' <- get
                                     -- inform all listeners about change
                                     lift $ sequence_ $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate update) (listeners cs')
                                     -- test winning condition
                                     when (isWinningState s') $
                                         lift $ sequence_ $ map (flip runReaderT (scenarioState cs') . notifyWin) (listeners cs')
                                     return Nothing
    addListener l = do modify (\cs -> cs { listeners = UpdateListenerType l : listeners cs })
                       cs <- get
                       lift $ runReaderT (notifyNew l) (scenarioState cs)
                       return ()
    setScenario sc = do modify (\cs -> cs { scenarioState = sc })
                        cs <- get
                        lift $ sequence_ $ map (flip runReaderT (scenarioState cs) . notifyNew) (listeners cs)
                        return ()
    undoAction = do cs <- get
                    case undo (scenarioState cs) of
                         Left r -> (return . Just) r
                         Right (u, scs) -> do put (cs { scenarioState = scs })
                                              cs' <- get
                                              lift $ sequence_ $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate u) (listeners cs)
                                              return Nothing
    redoAction = do cs <- get
                    case redo (scenarioState cs) of
                         Left r -> (return . Just) r
                         Right (u, scs) -> do put (cs { scenarioState = scs })
                                              cs' <- get
                                              lift $ sequence_ $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate u) (listeners cs)
                                              return Nothing

