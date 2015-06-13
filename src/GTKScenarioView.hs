module GTKScenarioView where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B
import           Graphics.UI.Gtk

import Scenario
import ScenarioController

createTextViewLink :: TextBuffer -> UpdateListener MatrixScenario
createTextViewLink = UpdateListener . textViewListenerFunction

textViewListenerFunction :: TextBuffer -> ScenarioUpdate -> ReaderT (ScenarioState MatrixScenario) IO ()
textViewListenerFunction tBuffer _ = do scState <- ask -- todo: player position
                                        liftIO $ putStrLn "listener called"
                                        (liftIO . textBufferSetByteString tBuffer . B.pack . showScenario . scenario) scState