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
                                        let (pX, pY) = playerCoord scState
                                            sc       = scenario scState
                                            levelStr = showScenario sc
                                            ((xl, yl), (xh, _)) = getMatrixScenarioBounds sc
                                            -- (#cols + 1) * (pY-yl) + (pX-xl)
                                            playerCLinear = (xh - xl + 2) * (pY - yl) + (pX - xl)
                                            (pre', succ') = B.splitAt (playerCLinear + 1) levelStr
                                            levelStrWithPlayer = B.append (B.init pre') ((combinePlayerAndFeature . B.index levelStr) playerCLinear `B.cons` succ')
                                        (liftIO . textBufferSetByteString tBuffer) levelStrWithPlayer

combinePlayerAndFeature :: Char -> Char
combinePlayerAndFeature '.' =  '+'    -- Target
combinePlayerAndFeature ' ' =  '@'    -- Floor
combinePlayerAndFeature _   =  '@'    -- others are invalid, fall back