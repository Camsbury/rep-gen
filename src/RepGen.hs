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
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (get, runStateT)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN)

printThing :: RGM ()
printThing = lift $ print "thing"

-- | Build a chess repertoire from a config
buildRepertoire :: RGConfig -> IO ()
buildRepertoire rgConfig
  = void
  . runStdoutLoggingT
  . runExceptT
  . (`runReaderT` rgConfig)
  . (`runStateT` RGState)
  $ do
    logInfoN "logging!"
