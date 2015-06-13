{-# LANGUAGE ExistentialQuantification #-}

module ScenarioController where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import Scenario

newtype UpdateListener sc = UpdateListener { notifyUpdate :: ScenarioUpdate -> ReaderT (ScenarioState sc) IO () }

data ControllerState sc = ControllerState { scenarioState :: ScenarioState sc
                                          , listeners     :: [UpdateListener sc] }


instance Show sc => Show (ControllerState sc) where
    show (ControllerState scen lst) = "ControllerState {scenarioState = " ++ show scen ++ ", listeners = (" ++ (show . length) lst ++ "listeners )}"


-- | Initial controller state with the given scenario and without any liseners.
initControllerState :: ScenarioState sc -> ControllerState sc
initControllerState sc = ControllerState sc []

-- | Adds a listener to be notified about a changed scenario.
addListener :: UpdateListener sc -> State (ControllerState sc) ()
addListener l = do modify (\cs -> cs { listeners = l : listeners cs })

-- | Tries to move the player into the specified direction.
--   Notifies all registered listeners on success and returns 'Nothing'.
--   If the move is not possible returns the reason.
runPlayerMove :: Scenario sc => PlayerMovement -> StateT (ControllerState sc) IO (Maybe DenyReason)
runPlayerMove move = do cs <- get
                        let s = scenarioState cs
                        case askPlayerMove move s of
                             (Left reason) -> (return . Just) reason -- propagate reason for invalid move
                             (Right update) -> do -- update internal ScenarioState
                                                  put $ cs { scenarioState = updateScenario s update }
                                                  cs <- get
                                                  -- inform all listeners about change
                                                  liftIO $ sequence_ $ map (flip runReaderT (scenarioState cs) . flip notifyUpdate update) (listeners cs)
                                                  return Nothing
