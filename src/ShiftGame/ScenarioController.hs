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

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import ShiftGame.Scenario

-- | A listener that can be informed about @ScenarioUpdate@s.
newtype UpdateListener m sc = UpdateListener { notifyUpdate :: ScenarioUpdate -> ReaderT (ScenarioState sc) m () }

-- | The state of a controller handling communication between model and view
data ControllerState m sc = ControllerState { scenarioState :: ScenarioState sc     -- ^ the game state
                                            , listeners     :: [UpdateListener m sc]  -- ^ all known listeners
                                            }


instance Show sc => Show (ControllerState m sc) where
    show (ControllerState scen lst) = "ControllerState {scenarioState = " ++ show scen ++ ", listeners = (" ++ (show . length) lst ++ "listeners )}"


-- | Initial controller state with the given scenario and without any liseners.
initControllerState :: Monad m => ScenarioState sc -> ControllerState m sc
initControllerState sc = ControllerState sc []

-- | Adds a listener to be notified about a changed scenario.
addListenerM :: Monad m => UpdateListener m sc -> StateT (ControllerState m sc) m ()
addListenerM l = do modify (\cs -> cs { listeners = l : listeners cs })

-- | Adds a listener to be notified about a changed scenario.
addListener :: Monad m => ControllerState m sc -> UpdateListener m sc -> ControllerState m sc
addListener cs l = cs { listeners = l : listeners cs }

-- | Tries to move the player into the specified direction.
--   Notifies all registered listeners on success and returns 'Nothing'.
--   If the move is not possible returns the reason.
runPlayerMove :: (Monad m, Scenario sc) => PlayerMovement -> StateT (ControllerState m sc) m (Maybe DenyReason)
runPlayerMove move = do cs <- get
                        let s = scenarioState cs
                        case askPlayerMove s move of
                             (Left reason) -> (return . Just) reason -- propagate reason for invalid move
                             (Right update) -> do -- update internal ScenarioState
                                                  put $ cs { scenarioState = updateScenario s update }
                                                  cs <- get
                                                  -- inform all listeners about change
                                                  lift $ sequence_ $ map (flip runReaderT (scenarioState cs) . flip notifyUpdate update) (listeners cs)
                                                  return Nothing
