{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances, InstanceSigs #-}
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
data UpdateListener m sc = UpdateListener { notifyUpdate :: ScenarioUpdate -> ReaderT (ScenarioState sc) m ()
                                          , notifyNew    :: ReaderT (ScenarioState sc) m ()
                                          , notifyWin    :: ReaderT (ScenarioState sc) m ()
                                          }


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
    addListener :: UpdateListener m sc -> StateT ctrl m ()
    -- | Aborts the current game and sets a new scenario.
    setScenario :: ScenarioState sc -> StateT ctrl m ()
    -- | Reverts a single step, returns @Nothing@ on success. Optional.
    undoAction :: StateT ctrl m (Maybe DenyReason)
    undoAction = return (Just ActionUnsupported)
    -- | Reverts a single step, returns @Nothing@ on success. Optional.
    redoAction :: StateT ctrl m (Maybe DenyReason)
    redoAction = return (Just ActionUnsupported)


initController :: (Functor m, ScenarioController ctrl sc m) => ScenarioState sc -> UpdateListener m sc -> m ctrl
initController sc lst = fmap snd $ runStateT (addListener lst) (initControllerState sc)

-- | The state of a controller handling communication between model and view
data ControllerState m sc = ControllerState { scenarioState :: ScenarioState sc       -- ^ the game state
                                            , listeners     :: [UpdateListener m sc]  -- ^ all known listeners
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
                                 (Right (_, update)) -> do                  -- update internal ScenarioState
                                     let (Right s') = updateScenario s update
                                     put $ cs { scenarioState = s' }
                                     cs' <- get
                                     -- inform all listeners about change
                                     lift $ sequence_ $ map (flip runReaderT (scenarioState cs') . flip notifyUpdate update) (listeners cs')
                                     -- test winning condition
                                     when (isWinningState s') $
                                         lift $ sequence_ $ map (flip runReaderT (scenarioState cs') . notifyWin) (listeners cs')
                                     return Nothing
    addListener l = do modify (\cs -> cs { listeners = l : listeners cs })
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
                                              lift $ sequence_ $ map (flip runReaderT (scenarioState cs) . flip notifyUpdate u) (listeners cs)
                                              return Nothing
    redoAction = do cs <- get
                    case redo (scenarioState cs) of
                         Left r -> (return . Just) r
                         Right (u, scs) -> do put (cs { scenarioState = scs })
                                              lift $ sequence_ $ map (flip runReaderT (scenarioState cs) . flip notifyUpdate u) (listeners cs)
                                              return Nothing

