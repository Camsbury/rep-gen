module RepGen
  ( module RepGen.PyChess
  , module RepGen.Type
  , module RepGen.Config.Type
  , module RepGen.Monad
  , buildRepertoire
  ) where
import Prelude
import RepGen.PyChess
import RepGen.Type
import RepGen.Config.Type
import RepGen.Monad
import Control.Lens.Combinators
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, runStateT)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN)
import Text.Pretty.Simple

-- | Build the move tree for the repertoire
buildTree :: RGM MoveTree
buildTree = undefined

-- | Build a chess repertoire from a config
buildRepertoire :: RGConfig -> IO ()
buildRepertoire rgConfig
  = void
  . runStdoutLoggingT
  . runExceptT
  . (`runReaderT` rgConfig)
  . (`runStateT` RGState)
  $ do
    tree <- buildTree
    cfg <- ask
    pPrintDarkBg cfg

